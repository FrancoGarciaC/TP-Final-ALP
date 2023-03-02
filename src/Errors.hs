module Errors where

import Text.Parsec.Error ( ParseError )

data Error = ParseErr ParseError
           | ActionError String

instance Show Error where
    show (ParseErr e) = show e
    show (ActionError e) = show e
