{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Text.Printf         ( printf )
import           Control.Arrow       ( first, second )
import           Control.Monad
import           Data.List           ( intercalate )
import           Data.Aeson
import           System.Environment  ( getArgs )
import           System.Exit         ( exitWith, ExitCode(..) )

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict        as M

data Build = Build
    { name         :: String
    , version      :: String
    , description  :: String
    , license      :: String
    , author       :: String
    , contributors :: [String]
    , repository   :: String
    , dependencies :: [(Source, String)]
    } deriving Show

instance FromJSON Build where
    parseJSON (Object v)
      = Build <$> v .: "name"
              <*> v .: "version"
              <*> v .: "description"
              <*> v .: "license"
              <*> v .: "author"
              <*> v .: "contributors"
              <*> v .: "repository"
              <*> (map (first read) . M.toList <$> v .: "dependencies")

    parseJSON _ = mzero

type Derivation = String

elm2nix :: Build -> Derivation
elm2nix b
  = let
        fmt  = unlines [ "{ elm, fetchgit%s }:"
                       , ""
                       , "elm.mkDerivation {"
                       , "  name = \"%s\";"
                       , "  version = \"%s\";"
                       , "  src = fetchgit {"
                       , "    url = %s;"
                       , "    rev = \"%s\";"
                       , "  };"
                       , "  buildDepends = [ %s ];"
                       , "}" ]

        args | null deps = ""
             | otherwise = intercalate ", " ([]:deps)
        nam  = name b
        ver  = version b
        url  = repository b
        rev  = mkRev (version b)
        inps = intercalate " " deps

        deps :: [String]
        deps = map mkAttr (dependencies b)

        mkAttr ((Central s), v)   = printf "\"%s%s\"" s (mkVer v)
        mkAttr ((GitHub  s t), v) = printf "\"%s\".\"%s%s\"" s t (mkVer v)

        mkVer "*"      = mkVer "master"
        mkVer "master" = ""
        mkVer s        = '_' : map (\c -> if c == '.' then '_' else c) s

        mkRev ""  = mkRev "master"
        mkRev "*" = mkRev "master"
        mkRev s | isDigit (head s) = "refs/tags/"  ++ s
                | otherwise        = "refs/heads/" ++ s

        isDigit = flip elem ['0'..'9']

    in printf fmt args nam ver url rev inps

data Source = GitHub String String | Central String

instance Read Source where
    readsPrec _ s = let (r, t) = second (drop 1) $ break (=='/') s
                    in if null t then [(Central r, "")] else [(GitHub r t, "")]

instance Show Source where
    show (GitHub  s t) = s ++ "/" ++ t
    show (Central s)   = s

main :: IO ()
main = do
  args <- getArgs
  case args of
      [fn] -> do
          result <- decode <$> B.readFile fn
          case result of
              Just b  -> do putStrLn (elm2nix b)
                            exitWith ExitSuccess
              Nothing -> exitWith (ExitFailure 1)
      _    -> putStrLn "elm2nix build.json > default.nix"

