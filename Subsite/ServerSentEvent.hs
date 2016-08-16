{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Subsite.ServerSentEvent where

import           Subsite.ServerSentEvent.Data
import           Yesod

instance YesodServerSentEvent master => YesodSubDispatch ServerSentEvent (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesServerSentEvent)


toasterWidget :: YesodServerSentEvent master
           => (Route ServerSentEvent -> Route master)
           -> WidgetT master IO ()
toasterWidget toMaster = do
    ili <- handlerToWidget isLoggedIn  -- check if we're already logged in
    if ili
            then do
                -- Logged in: show the widget
                [whamlet|
                    <div ##{chat}>
                        <h2>ServerSentEvent
                        <div ##{output}>
                        <input ##{input} type=text placeholder="Enter Message">
                |]
                -- Just some CSS
                toWidget [lucius|
                    ##{chat} {
                        position: absolute;
                        top: 2em;
                        right: 2em;
                    }
                    ##{output} {
                        width: 200px;
                        height: 300px;
                        border: 1px solid #999;
                        overflow: auto;
                    }
                |]
                -- And now that Javascript
                toWidgetBody [julius|
                    // Set up the receiving end
                    var output = document.getElementById(#{toJSON output});
                    var src = new EventSource("@{toMaster ReceiveR}");

                    src.addEventListener('wiki-update', function(msg) {
                      updateServerSentEvent(msg);
                    });

                    src.onmessage = function(msg) {
                      updateServerSentEvent(msg);
                    };

                    updateServerSentEvent = function(msg) {
                      // Msg can now be easily parsed.
                      var text = JSON.parse(msg.data);
                      // This function will be called for each new message.
                      var p = document.createElement("p");
                      p.appendChild(document.createTextNode(msg.data));
                      output.appendChild(p);

                      // And now scroll down within the output div so the most recent message
                      // is displayed.
                      output.scrollTop = output.scrollHeight;
                    };

                    // Set up the sending end: send a message via Ajax whenever the user hits
                    // enter.
                    var input = document.getElementById(#{toJSON input});
                    input.onkeyup = function(event) {
                        var keycode = (event.keyCode ? event.keyCode : event.which);
                        if (keycode == '13') {
                            var xhr = new XMLHttpRequest();
                            var val = input.value;
                            input.value = "";
                            var params = "?message=" + encodeURI(val);
                            xhr.open("POST", "@{toMaster SendR}" + params);
                            xhr.send(null);
                        }
                    }
                |]
    else do
        -- User isn't logged in, give a not-logged-in message.
        master <- getYesod
        [whamlet|
            <p>
                You must be #
                $maybe ar <- authRoute master
                    <a href=@{ar}>logged in
                $nothing
                    logged in
                \ to chat.
        |]
