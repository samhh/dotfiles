module App where

import           Workspaces (WorkspaceId (..))

data App
  = Discord
  | Slack
  deriving Enum

apps :: [App]
apps = [toEnum 0 ..]

className :: App -> String
className Discord = "discord"
className Slack   = "Slack"

workspaceOf :: App -> WorkspaceId
workspaceOf Discord = W9
workspaceOf Slack   = W8

