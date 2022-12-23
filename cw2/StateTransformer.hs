module StateTransformer(StateIO(..), appIO, lift, stStateIO, stUpdateIO, StateTransformer(..), app, stState, stUpdate) where

-- State Transformer
newtype StateTransformer st a = S(st -> (a, st))

app :: StateTransformer st a -> st -> (a, st)
app (S f) x = f x

stState :: StateTransformer st st
stState = S(\st -> (st, st))

stUpdate st = S(\_ -> ((), st))

instance Functor (StateTransformer st) where
-- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative (StateTransformer st) where
-- pure :: a -> ST a
    pure x = S (\s -> (x,s))
-- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f,s') = app stf s
            (x,s'') = app stx s' in (f x, s''))

instance Monad (StateTransformer st)where
-- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


-- IO State transformer
newtype StateIO st a = StT(st -> IO(a, st))

appIO :: StateIO st a -> st -> IO(a, st)
appIO (StT stf) s = stf s

lift :: IO a -> StateIO st a
lift mx = StT(\st -> do
     x <- mx
     return (x, st))

stStateIO :: StateIO st st
stStateIO = StT(\st -> return (st, st))

stUpdateIO st = StT(\_ -> return((), st))

instance Functor (StateIO st) where
    fmap g sta = StT(\st -> do 
        (x, st') <- appIO sta st 
        return (g x, st'))    


instance Applicative (StateIO st) where
    pure x = StT(\s -> return (x, s))
    stf <*> stx = StT(\s -> do
        (f, s') <- appIO stf s
        (x, s'') <- appIO stx s'
        return (f x, s''))

instance Monad (StateIO st) where
    st >>= f = StT(\s -> do
        (x, s') <- appIO st s
        (y, s'') <- appIO (f x) s'
        return (y, s'') )