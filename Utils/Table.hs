module Utils.Table
  ( buildSemanticUi
  ) where

import Import
import Yesod.Table

-- This is a straight copy/ paste from Yesod.Table's buildBootstrap, with the
-- table classes adapted to Semantic UI.
buildSemanticUi :: Table site a -> [a] -> WidgetT site IO ()
buildSemanticUi (Table cols) vals = table $ do
  thead $ mapM_ header cols
  tbody $ forM_ vals $ \val -> tr $ forM_ cols $ \col -> cell col val
  where table b  = asWidgetIO [whamlet|
                     <table.ui.celled.table>^{b}
                   |]
        thead b  = asWidgetIO [whamlet|
                     <thead>
                       <tr>
                         ^{b}
                   |]
        tbody b  = asWidgetIO [whamlet|
                     <tbody>^{b}
                   |]
        tr b     = asWidgetIO [whamlet|
                     <tr>^{b}
                   |]

asWidgetIO :: WidgetT site IO () -> WidgetT site IO ()
asWidgetIO = id
