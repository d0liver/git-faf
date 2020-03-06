module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (message)
import Node.ChildProcess (ExecResult, defaultExecOptions, exec)
import Node.Process (exit)
import Options.Applicative.Builder (forwardOptions, header, info, progDesc)
import Options.Applicative.Extra (execParser, helper)

bailIf :: Effect Unit -> ExecResult -> Effect Unit
bailIf c r =
  case r.error of
    Just err -> log (message err) *> exit 1
    _ -> c

push :: Effect Unit -> Effect Unit
push c = pure unit <* exec "git push" defaultExecOptions (bailIf c)

delete :: Effect Unit -> Effect Unit
delete c = pure unit <* exec "git branch -D" defaultExecOptions (bailIf c)

main :: Effect Unit
main = do
  opts <- execParser $ info helper (
    header "git faf"
    <> progDesc "'Fire and forget' to push a branch and immediately delete it locally."
    <> forwardOptions
  )
  push (delete (pure unit))
