module Contents.Contest where

import Import

renderAzunyan :: Int -> WidgetT App IO ()
renderAzunyan size = [whamlet| <img src=@{StaticR img_azunyan_jpg} width=#{size}> |] 