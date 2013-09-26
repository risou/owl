module Handler.Owl where
import Import
import qualified Data.Text as T
import Data.Time
import Text.Pandoc
import Yesod.Auth

getRootR :: Handler RepHtml
getRootR = do
	maid <- maybeAuthId
	blogTitle <- getBlogTitle
	entries <- runDB $ selectList [] [LimitTo 5, Desc EntryPosted]
	defaultLayout $ do
		setTitle $ toHtml $ blogTitle
		$(widgetFile "owl")

getListR :: Handler RepHtml
getListR = do
	blogTitle <- getBlogTitle
	entries <- runDB $ selectList [] [Desc EntryPosted]
	defaultLayout $ do
		setTitle $ toHtml $ T.concat ["entry list - ", blogTitle]
		$(widgetFile "list")

getEntryR :: EntryId -> Handler RepHtml
getEntryR entryId = do
	blogTitle <- getBlogTitle
	entry <- runDB $ get404 entryId
	defaultLayout $ do
		setTitle $ toHtml $ T.concat [entryTitle entry, " - ", blogTitle]
		$(widgetFile "entry")

getNewR :: Handler RepHtml
getNewR = do
	raid <- requireAuthId
	(widget, enctype) <- generateFormPost $ entryForm Nothing
	defaultLayout $ do
		$(widgetFile "new")

postNewR :: Handler RepHtml
postNewR = do
	raid <- requireAuthId
	((res, widget), enctype) <- runFormPost $ entryForm Nothing
	case res of
		FormSuccess entryBase -> do
			entryId <- runDB $ insert $ Entry {
				entryTitle = (title entryBase),
				entryPosted = (posted entryBase),
				entryMdContent = (content entryBase),
				entryHtmlContent = toHtml $ writeHtml def $ readMarkdown def $ T.unpack $ replaceLF $ unTextarea (content entryBase)
			}
			redirect $ EntryR entryId
		_ -> defaultLayout $ do
			setTitle "Please correct your entry form"
			$(widgetFile "new")

data EntryBase = EntryBase {
	title :: Text,
	posted :: ZonedTime,
	content :: Textarea
}

entryForm :: Maybe Entry -> Form EntryBase
entryForm e = renderDivs $ EntryBase
	<$> areq textField "Title" (entryTitle <$> e)
	<*> aformM (liftIO getZonedTime)
	<*> areq textareaField "Content" (entryMdContent <$> e)

getEditR :: EntryId -> Handler RepHtml
getEditR entryId = do
	raid <- requireAuthId
	entryBase <- runDB $ get404 entryId
	(widget, enctype) <- generateFormPost $ entryForm $ Just entryBase
	defaultLayout $ do
		$(widgetFile "new")

postEditR :: EntryId -> Handler RepHtml
postEditR entryId = do
	raid <- requireAuthId
	((res, widget), enctype) <- runFormPost $ entryForm Nothing
	case res of
		FormSuccess entryBase -> do
			runDB $ do
				_entry <- get404 entryId
				update entryId [
					EntryTitle =. title entryBase,
					EntryMdContent =. content entryBase,
					EntryHtmlContent =. (toHtml $ writeHtml def $ readMarkdown def $ T.unpack $ replaceLF $ unTextarea (content entryBase))]
			redirect $ EntryR entryId
		_ -> defaultLayout $ do
			setTitle "Please correct your entry form"
			$(widgetFile "new")

getDeleteR :: EntryId -> Handler RepHtml
getDeleteR entryId = do
	_ <- requireAuthId
	runDB $ do
		_entry <- get404 entryId
		delete entryId
	redirect $ RootR

replaceLF :: Text -> Text
replaceLF s = T.replace (T.pack "\r\n") (T.pack "\n") s
