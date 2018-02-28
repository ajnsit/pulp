module Pulp.Dep
  ( action
  ) where

import Data.Either
import Data.Maybe
import Prelude
import Pulp.Args
import Pulp.Args.Get
import Pulp.Outputter
import Pulp.System.FFI

import Control.Monad.Aff (makeAff, launchAff, attempt)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Foreign (Foreign)
import Data.Foreign (toForeign)
import Data.Generic (class Generic, gShow)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith, trim, lastIndexOf, Pattern(..), take, drop)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.HTTP as HTTP
import Node.Process as Process
import Node.Stream as Stream
import Pulp.Build as Build
import Pulp.DepCommand (DepCommand(..), parseDepCommand)
import Pulp.Exec (execQuiet)
import Pulp.System.Files as Files
import Pulp.System.StaticServer as StaticServer
import Pulp.System.Stream (write, end, WritableStream, stdout)
import Pulp.System.Which (which)
import Pulp.Utils (orErr)
import Pulp.Utils (throw)
import Pulp.Watch (watchAff, watchDirectories)

preludePackageName :: String
preludePackageName = "prelude"

untitledPackageName :: String
untitledPackageName = "untitled"

action :: Action
action = Action $ \args -> do
  out <- getOutputter args
  mdepCommand <- getOption "command" args.commandArgs
  case mdepCommand of
    Just cmd -> do
      case parseDepCommand cmd of
        Just Init -> doInit out
        Just Install -> doInstall out
        Just CommandList -> doCommandList out
        _ -> throwError <<< error $
          "\"" <> cmd <> "\" is not a valid dep command. Must be one of: " <>  joinWith ", " (map _.name commands) <> "."
    -- This will never happen because we use a string passthrough parser which never fails
    -- But we handle it anyways
    Nothing -> throwError <<< error $
           "Invalid arguments passed to pulp dep" <> show (args.remainder) <> "."

doCommandList :: forall e. { log :: String -> AffN Unit | e } -> AffN Unit
doCommandList out = out.log $ "COMMAND can be one of the following - \n "
    <> joinWith "\n " (map (\c -> c.name <> " - " <> c.desc) commands)

doInstall :: forall e. { log :: String -> AffN Unit | e } -> AffN Unit
doInstall out = do
  out.log "Installing... DONE"

doInit :: forall e. { log :: String -> AffN Unit | e } -> AffN Unit
doInit out = do
  exists <- FS.exists "psc-package.json"
  when exists $ throw "psc-package.json already exists"

  pwd <- liftEff Process.cwd
  out.log $ "Initializing new project in the current directory - " <> pwd

  let pkgName = getFilename pwd

  pursVersion <- trim <$> execQuiet "purs" ["--version"] Nothing
  pursPath <- attempt $ which "purs"
  out.log $
    "purs version " <> pursVersion <>
    either (const "") (\p -> " using " <> trim p) pursPath

  let config = joinWith "\n"
        [ "{ "
        , "  \"name\"    : \"" <> pkgName <> "\","
        , "  \"depends\" : [ \"" <> preludePackageName <> "\" ],"
        , "  \"source\"  : \"https://github.com/purescript/package-sets.git\","
        , "  \"set\"     : \"psc-" <> pursVersion <> "\""
        , "}"
        ]
  let outFile = pwd <> "/psc-package.json"
  out.log $ "Writing configuration to " <> outFile
  res <- attempt do
          stream <- liftEff $ Files.createWriteStream outFile
          void $ write stream config
          void $ end stream
  case res of
    Right _ -> pure unit
    Left err -> throwError err

  out.log "Done"

getFilename :: String -> String
getFilename s = case lastIndexOf (Pattern "/") s of
  Just x -> drop (x+1) s
  Nothing -> s

getFilepath :: String -> String
getFilepath s = case lastIndexOf (Pattern "/") s of
  Just x -> take x s
  Nothing -> s

commands :: Array {name :: String, desc :: String}
commands =
  [ { name : "init", desc : "Create a new psc-package.json file"}
  , { name : "uninstall", desc : "Uninstall the named package"}
  , { name : "install", desc : "Install/update the named package and add it to 'depends' if not already listed. If no package is specified, install/update all dependencies."}
  , { name : "build", desc : "Install dependencies and compile the current package"}
  , { name : "repl", desc : "Open an interactive environment for PureScript"}
  , { name : "dependencies", desc : "List all (transitive) dependencies for the current package"}
  , { name : "sources", desc : "List all (active) source paths for dependencies"}
  , { name : "available", desc : "List all packages available in the package set"}
  , { name : "updates", desc : "Check all packages in the package set for new releases"}
  , { name : "verify", desc : "Verify that the named package builds correctly. If no package is specified, verify that all packages in the package set build correctly."}
  , { name : "format", desc : "Format the packages.json file for consistency"}
  , { name : "commandlist", desc : "List all commands which can be passed to `pulp dep`"}
  ]
