
-------------------
-- Very basic things
-------------------
foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap

--------------
-- Helpers for stealing JSON instances from an isomorphic type
-------------
isoParseJSON :: FromJSON a => Iso' b a -> Value -> Parser b
isoParseJSON i v = view (mapping (from i)) (parseJSON v)

isoToJSON :: ToJSON a => Iso' b a -> b -> Value
isoToJSON i b = toJSON (view i b)

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

