
-------------------
-- Very basic things
-------------------
foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap

readsMaybe :: ReadS a -> String -> Maybe a
readsMaybe f s = case filter (null . snd) (f s) of
  ((a, _):_) -> Just a
  _ -> Nothing

--------------
-- Helpers for stealing JSON instances from an isomorphic type
-------------
isoParseJSON :: FromJSON a => Iso' b a -> Value -> Parser b
isoParseJSON i v = view (mapping (from i)) (parseJSON v)

isoToJSON :: ToJSON a => Iso' b a -> b -> Value
isoToJSON i b = toJSON (view i b)

prismParseJSON :: FromJSON a => Prism' a b -> Value -> Parser b
prismParseJSON p v = parseJSON v >>= \a -> case a ^? p of
  Nothing -> fail "representation conversion was unsuccessful"
  Just b  -> return b

prismToJSON :: ToJSON a => Prism' a b -> b -> Value
prismToJSON p b = toJSON $ withPrism p $ \f _ -> f b

-------------------
-- Helpers for automatically generating PersistField instances
-------------------

toPersistValueJSON :: ToJSON a => a -> PersistValue
toPersistValueJSON = PersistText . Text.decodeUtf8 . LByteString.toStrict . encode

fromPersistValueJSON :: FromJSON a => PersistValue -> Either Text a
fromPersistValueJSON (PersistByteString bs) = case decode (LByteString.fromStrict bs) of
  Nothing -> Left "Could not parse the JSON"
  Just x -> Right x
fromPersistValueJSON (PersistText t) = case decodeStrict (Text.encodeUtf8 t) of
  Nothing -> Left "Could not parse the JSON"
  Just x -> Right x
fromPersistValueJSON a = Left $ "Expected PersistByteString, received: " <> Text.pack (show a)


---------------------------
-- Helpers for building yesod forms
--------------------------

mapField :: Monad m => (a -> b) -> (b -> Either (SomeMessage (HandlerSite m)) a) -> Field m b -> Field m a
mapField fwd bck (Field parse view enctype) = Field
  (\ts fis -> do
     eres <- parse ts fis 
     return $ eres >>= (\mb -> case mb of
       Just b  -> Just <$> bck b 
       Nothing -> Right Nothing)
  )
  (\a b c d e -> view a b c (fmap fwd d) e)
  enctype

mapField :: Monad m => (a -> b) -> (b -> Either (SomeMessage (HandlerSite m)) a) -> Field m b -> Field m a
mapField fwd bck (Field parse view enctype) = Field
  (\ts fis -> do
     eres <- parse ts fis 
     return $ eres >>= (\mb -> case mb of
       Just b  -> Just <$> bck b 
       Nothing -> Right Nothing)
  )
  (\a b c d e -> view a b c (fmap fwd d) e)
  enctype

yamlFieldWithHelp :: (FromJSON a, ToJSON a, Monad m, RenderMessage (HandlerSite m) FormMessage) 
                  => a -> Field m a
yamlFieldWithHelp example = appendFieldView w $ mapField fwd bck textareaField'
  where fwd = TE.decodeUtf8 . Yaml.encode 
        bck = mapLeft (SomeMessage . Text.pack) . Yaml.decodeEither . TE.encodeUtf8
        w   = do
          buttonId <- newIdent
          preId <- newIdent
          $(widgetFile "yaml-field-help")
        encodedExample = TE.decodeUtf8 $ Yaml.encode example

yamlField :: (FromJSON a, ToJSON a, Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m a
yamlField = mapField fwd bck textareaField'
  where fwd = TE.decodeUtf8 . Yaml.encode 
        bck = mapLeft (SomeMessage . Text.pack) . Yaml.decodeEither . TE.encodeUtf8

textareaField' :: forall m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text
textareaField' = f { fieldParse = fieldParse f
                   , fieldView = \a b c d e -> fieldView f a b (addDefRows c) d e :: WidgetT (HandlerSite m) IO () }
  where addDefRows xs = case lookup "rows" xs of
          Nothing -> ("rows","5") : xs
          Just _ -> xs
        f = mapField Textarea (Right . unTextarea) (textareaField :: Field m Textarea)

textareaFieldLarge :: forall m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Text
textareaFieldLarge = f { fieldParse = fieldParse f
                       , fieldView = \a b c d e -> fieldView f a b (addDefRows c) d e :: WidgetT (HandlerSite m) IO () }
  where addDefRows xs = case lookup "rows" xs of
          Nothing -> ("rows","20") : xs
          Just _ -> xs
        f = mapField Textarea (Right . unTextarea) (textareaField :: Field m Textarea)

appendFieldView :: WidgetT (HandlerSite m) IO () -> Field m a -> Field m a
appendFieldView w (Field parse view enctype) = Field parse (\a b c d e -> view a b c d e >> w) enctype

bft :: Text -> FieldSettings site
bft = bfs

------------------
-- Wrapping automatically instead of giving the user an enctype to deal with
------------------

runFormPostBuild :: RenderMessage site FormMessage => Route site -> (Html -> MForm (HandlerT site IO) (FormResult a, WidgetT site IO ())) -> HandlerT site IO (FormResult a, WidgetT site IO ())
runFormPostBuild route form = do
  ((res, widget), enctype) <- runFormPost form
  return (res, formWrap route widget enctype "post")

runFormGetBuild :: Route site -> (Html -> MForm (HandlerT site IO) (FormResult a, WidgetT site IO ())) -> HandlerT site IO (FormResult a, WidgetT site IO ())
runFormGetBuild route form = do
  ((res, widget), enctype) <- runFormGet form
  return (res, formWrap route widget enctype "get")

formWrap :: Route site -> WidgetT site IO () -> Enctype -> Text -> WidgetT site IO ()
formWrap route widget enctype method = [whamlet|$newline never
<form action="@{route}" enctype="#{enctype}" method="#{method}">
  ^{widget}
|]

------------------
-- Random other yesod stuff
------------------

selectEnum :: (Eq a, Enum a, Bounded a, RenderMessage site FormMessage, RenderMessage site a) 
           => Field (HandlerT site IO) a
selectEnum = selectFieldList $ map (\a -> (a,a)) [minBound..maxBound] 

submitButton :: MonadHandler m => AForm m ()
submitButton = bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

deleteButton :: MonadHandler m => AForm m ()
deleteButton = bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn-danger" [])

