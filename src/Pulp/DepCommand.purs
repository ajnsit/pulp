-- | Defines a DepCommand type and associated functions.
module Pulp.DepCommand where

import Data.Maybe (Maybe(..))
import Data.String as String
import Prelude (class Show)

data DepCommand
  = Init
  | Uninstall
  | Install
  | Build
  | Repl
  | Dependencies
  | Sources
  | Available
  | Updates
  | Verify
  | Format
  | CommandList

instance showDepCommand :: Show DepCommand where
  show Init = "Init"
  show Uninstall = "Uninstall"
  show Install = "Install"
  show Build = "Build"
  show Repl = "Repl"
  show Dependencies = "Dependencies"
  show Sources = "Sources"
  show Available = "Available"
  show Updates = "Updates"
  show Verify = "Verify"
  show Format = "Format"
  show CommandList = "CommandList"

parseDepCommand :: String -> Maybe DepCommand
parseDepCommand str =
  case String.toLower str of
    "init" -> Just Init
    "uninstall" -> Just Uninstall
    "install" -> Just Install
    "build" -> Just Build
    "repl" -> Just Repl
    "dependencies" -> Just Dependencies
    "sources" -> Just Sources
    "available" -> Just Available
    "updates" -> Just Updates
    "verify" -> Just Verify
    "format" -> Just Format
    "commandlist" -> Just CommandList
    _ -> Nothing
