module Import
    ( module Import
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import JitCert.GSN
import Text.Blaze.Html.Renderer.Text
import Text.Julius

import GSN.GHC
import Foundation            as Import
import Import.NoFoundation   as Import


wrapErrorBlock :: Widget
wrapErrorBlock = 
    -- Hack for somewhat better GHC error rendering. 
    toWidget [lucius|
        .error-block {
            white-space: pre-wrap;
        }
    |]

-- https://gist.github.com/carymrobbins/590515bb8dfb48573527
bootstrapCheckBoxField :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => Text -> Field m Bool
bootstrapCheckBoxField label = checkBoxField
    { fieldView = \theId name attrs val _ -> [whamlet|\
        <div .checkbox style="margin: 0">
            <label>
                <input id=#{theId} *{attrs} type="checkbox" name=#{name} autocomplete="off" value=yes :showVal id val:checked> #{label}
        |]
    }
  where
    -- <div .checkbox style="margin-top: -20px; margin-bottom: 0">
    -- <label style="margin-bottom: 5px;">
    showVal = either $ const False

-- codeMirrorField :: forall m . (MonadIO m, RenderMessage (HandlerSite m) FormMessage) => Field m Textarea
-- codeMirrorField = textareaField {fieldView = fieldView (textareaField :: Field m Textarea)
codeMirrorField :: forall m . (MonadIO m, RenderMessage (HandlerSite m) FormMessage, HandlerSite m ~ App) => Field m Textarea
codeMirrorField = textareaField {fieldView = \fvId fvName fvAttrs fvRes fvReq -> do
        addStylesheet $ StaticR css_codemirror_css
        addScript $ StaticR js_codemirror_js
        addScript $ StaticR js_codemirror_haskell_js
        fieldView (textareaField :: Field m Textarea) fvId fvName fvAttrs fvRes fvReq :: WidgetFor (HandlerSite m) ()
        toWidget [julius|
            var myCodeMirror = CodeMirror.fromTextArea(document.getElementById(#{fvId}), {
              lineNumbers: true,
              mode: "haskell"
            });
            myCodeMirror.setSize( null, 450);
        |]
            -- (function() {
            -- })();
    }

-- Assumes the codeMirror has a global variable named `myCodeMirror`.
codeMirrorTemplateField :: (m ~ HandlerFor App) => [(Text,Text)] -> Field m Int
codeMirrorTemplateField ts = field {fieldView = \fvId fvName fvAttrs fvRes fvReq -> do
    fieldView field fvId fvName fvAttrs fvRes fvReq 
    toWidget [julius|
        (function() {
            var templates = #{Aeson.toJSON templateMap};
            var prev = #{toVal fvRes};
            $('#'+#{fvId}).change( function() {
                var k = $(this).val();
                if (k in templates) {
                    if ( myCodeMirror.isClean()) {
                        myCodeMirror.setValue( templates[k]);
                        myCodeMirror.markClean();
                        prev = k;
                    }
                    else {
                        if ( confirm("You have unsaved changes. Overwrite changes?")) {
                            myCodeMirror.setValue( templates[k]);
                            myCodeMirror.markClean();
                            prev = k;
                        }
                        else {
                            $(this).val( prev);
                            return false;
                        }
                    }
                }
            });
        })();
    |]
  }

  where
    toVal = toJSON . either (const 1) id

    field = selectFieldList $ zip (map fst ts) [1..]

    templateMap :: Map Int Text
    templateMap = Map.fromList $ zip [1..] $ map snd ts

gsnSourceField :: (MonadIO m, RenderMessage (HandlerSite m) FormMessage, HandlerSite m ~ App) => Field m (String, (GSNGraph, [(Int, Text, GSNEnv)]))
gsnSourceField = checkMMap compileSource (Textarea . Text.pack . fst) codeMirrorField
    
    where
        compileSource t = do
            let source = Text.unpack $ unTextarea t

            res <- compileGSN source

            case res of
                Left errs ->
                    -- return $ Left ("Invalid GSN source code.<br>TEST...." :: Text)
                    -- return $ Left $ toHtml $ Textarea errMsg
                    -- return $ Left errs
                    -- TODO: Highlight errors and warning in CodeMirror. 
                    -- TODO: Display newlines in errors.
                    return $ Left $ Text.unlines $ map snd errs
                Right gsn -> 
                    return $ Right (source, gsn)

withReadonly :: FieldSettings site -> FieldSettings site
withReadonly fs = fs { fsAttrs = ("readonly","readonly"):fsAttrs fs}

withDisabled :: FieldSettings site -> FieldSettings site
withDisabled fs = fs { fsAttrs = ("disabled","disabled"):fsAttrs fs}

addClassFieldSettings :: Text -> FieldSettings site -> FieldSettings site
addClassFieldSettings c fs = fs { fsAttrs = addClass c (fsAttrs fs)}

instance ToJavascript Html where
    toJavascript = toJavascript . renderHtml

instance ToJSON Html where
    toJSON = toJSON . renderHtml

