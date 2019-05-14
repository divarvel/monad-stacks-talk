% Application design with monad stacks
% Clément Delafargue
% ncrafts 2019-05-16

---

## FP in the small, OOP in the large

<details role="note">
today I'll try to show you why I think it's not a good motto
</details>

---

## FP: what is it good for?

<details role="note">
absolutely everything
properly model business code
but also, plumbing
</details>

---

## Today I'll talk about plumbing

---

## General web service design

<details role="note">
push IO and associated tasks at the outside
typical request / response flow diagram

no particular name given to it (as opposed to hex architecture)
</details>

---

## Model IO in types

<details role="note">
because as soon as you start to make IO explicit, it's the natural
thing to do.
</details>

---

```haskell
getHost :: IO String
getHost = getEnv "HOSTNAME"

main :: IO ()
main = do
  host <- getHost
  putStrLn host
```

---

```scala
// Using cats IO
def putStrLn(s: String): IO[Unit] =
  IO.apply(println(s))

def getHost(): IO[String] = {
  IO.apply(System.getEnv("HOSTNAME"))
}
```

---

```scala

def main(): IO[Unit] = for {
  host <- getHost()
  putStrLn(host)
}
```

---

```scala
def runMain(): Unit =
  main().unsafeRunSync
```

---

## Not only IO

<details role="note">
IO is of course a driving force, but it's not the only thing
</details>

---

## Not only IO

<details role="note">
schemas of the handling chain => multiple slides (add observability & DI)
</details>

---

# Combining IO steps

```haskell



main :: IO ()
main = do
  host <- getHost
  putStrLn host
```

---

# Combining IO steps

```haskell



main :: IO ()
main = getHost >>= (\host ->
         putStrLn host)
```
---

# Combining IO steps

```haskell




(>>=) :: IO a -> (a -> IO b) -> IO c
```
---

# Combining IO steps

```scala



def main(): IO[Unit] = for {
  host <- getHost()
  putStrLn(host)
}
```

---

# Combining IO steps

```scala



def main(): IO[Unit] = {
  getHost().flatMap(host => {
    putStrLn(host)
  })
```

---

# Combining IO steps

```scala




IO[A]#flatMap(f: A => IO[B]): IO[B]
```

---

# Modeling Errors

```haskell



data Either e a =
    Left e
  | Right a
```

---

# Combining Errors

```haskell



(>>=) :: Either e a
      -> (a -> Either e b)
      -> Either e b
```

---

```scala



Either[E,A]#flatMap(f: A => Either[E,B])
                             : Either[E,B]
```

---

## Let's talk DI

<details role="note">
Super important, driving force for app design in oop world
you could even create a framework just for DI
</details>

---

## DI is simple

<details role="note">
even though it's common to have runtime reflection, DI
containers and so on, DI at its core, is simple.

You need something, but it has to be provided
</details>

---

## DI is glorified functions

---

```haskell
cat :: Cheeseburger -> Nap
cat = \cheez ->
        -- whatever cats do
        )
```

---

```haskell
cat :: CanIHaz Cheeseburger Nap
cat = CanIHaz (\cheez ->
        -- whatever cats do
        )

newtype CanIHaz env a =
  CanIHaz (env -> a)
```

---

## Combining DI

<details role="note">
combining functions like this would be tedious
usually, the real dependency is deep down, and
flows upwards, but you don't want to apply functions
everywhere
</details>

---

# Combining DI

```haskell



(>>=) :: CanIHaz e a
      -> (a -> CanIHaz e b)
      -> CanIHaz e b
```

---

# Combining DI

```haskell



myOperation :: CanIHaz AppConfig Value
myOperation = do
  value1 <- myOperation1
  value2 <- myOperation2
  pure $ combine value1 value2
```

---

## Monads describe sequential composition

---

```haskell



forall m a b.
Monad m => m a -> (a -> m b) -> m b
```
---

```haskell



(<$>) :: (a -> b) ->       m a  -> m b
(>>=) :: m a      -> (a -> m b) -> m b
pure  :: a -> m a
```

---

# Laws

```haskell



fa >>= pure  === fa

pure a >>= f === f a

   (fa >>= f)         >>= g
=== fa >>= (\x -> f x >>= g)
```

<details role="note">
as long as you don't create monads, let the tooling
use them for you
</details>

---

## Plumbing is all about sequential composition

<details role="note">
Regular code is mostly regular composition. You only care about
business values. In plumbing code, the actual functions are usually
boring. What matters is the effects (error handling, etc)
</details>

---

## Combining effects

<details role="note">
Combining steps separately is nice, but usually I don't have
only one effect
async + DI + http errors

Can I compose them willy-nilly?
</details>

---

## Monads don't compose

---

```haskell


type Compose m n a = m (n a)

(>>=) :: (Monad m, Monad n)
      => Compose m n a
      -> (a -> Compose m n b)
      -> Compose m n b
```

---

## _Some_ monads compose <br> with _all_ monads

---

# Maybe

```haskell



(>>=) :: Monad m
      => m (Maybe a)
      -> (a -> m (Maybe b))
      -> m (Maybe b)
```

---

# Either

```haskell



(>>=) :: Monad m
      => m (Either e a)
      -> (a -> m (Either e b))
      -> m (Either e b)
```

---

# CanIHaz (aka Reader)

```haskell



(>>=) :: Monad m
      => Reader e (m a)
      -> (a -> Reader e (m b))
      -> Reader e (m b)
```
---

## Monad transformers

<details role="note">
take an arbitrary monad, and return a new
monad, extended with a specific effect
</details>

---

```haskell
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) =>
  Monad (MaybeT m) where
    (>>=) = -- fun exercise
```

---

```haskell


getSocket :: IO (Maybe String)
getSocket = runMaybeT $ do
  host <- MaybeT (lookupEnv "HOST")
  port <- MaybeT (lookupEnv "PORT")
  pure (host <> ":" <> port)
```


---

```scala
case class OptionT[F[_],A](

    value: F[Option[A]]

  ) {

    def flatMap[B](f: A => OptionT[F,B])
      : OptionT[F,B] = {
        // fun exercise
  }
}

```

---

```scala
def lookupEnv(s: String):
  IO[Maybe[String] =
    // todo

def getSocket(): IO[Maybe[String]] = (
  for {
    host <- OptionT(lookupEnv("HOST")
    port <- OptionT(lookupEnv("PORT")
  } yield s"${host}:${port}"
).value
```

---

## Combining more than two effects

<details role="note">
apply transformers one by one
monad stacks
</details>

---

# Example

```haskell



type Handler a =
  ExceptT Error IO a

type HandlerWithConf a =
  ReaderT Config Handler a
```

---

```haskell
findUser :: UserId
         -> HandlerWithConf User

findUser userId = do
  results <- runDB (findUser userId)
  case results of
    Just u  -> pure u
    Nothing -> throwError err404 -- tbd
```

---

# Example

```scala



type Handler[A] =
  ExceptT[Error,IO,A]

type HandlerWithConf[A] =
  ReaderT[Config,Handler,A]
```

---

```scala
findUser(userId: UserId)
  : HandlerWithConf[A]

= for {
  results <- runT(findUser(userId))
  response <- results match {
    case Some(u) => pure(u) //tdb
    case None => throwError(err404) //tbd
  }
} yield response
```

---

## Everything just needs to return `HandlerWithConf`

<details role="note">
Writing handlers is simplified, sure, but everything is tied
to the _whole_ monad stack
</details>

---

## "Program to an interface"

<details role="note">
Monad stacks are implementation, not interface
Not every function uses all the effects
</details>

---

## Making things declarative

<details role="note">
MTL-style
</details>

---

## Transformer classes

---

```haskell
-- for ReaderT
ask :: MonadReader e m => m e

-- for ExceptT
throwError :: MonadError e m => e -> m a

-- for IO
liftIO :: MonadIO m => IO a -> m a
```

---

```haskell



-- provided by a library
runDbTransaction :: Transaction a
                 -> Pool
                 -> IO a
```

---

```haskell
runDB :: ( Monad m
         , MonadReader Config m
         , MonadIO m)
      => Transaction a
      -> m a

runDB t = do
  pool <- asks dpPool
  liftIO (runDbTransaction t pool)
```

---

```haskell

findUser :: ( Monad m
            , MonadIO m
            , MonadReader Config m
            , MonadError Error m
            )
         => UserId
         -> m User
```

---

```haskell

findUser :: MonadEveryThing m
         => UserId
         -> m User

findUser uid = do
  results <- runTransaction $ findUser uid
  case results of
    Just u  -> pure u
    Nothing -> throwError err404
```

---

# Transformers as a free* implementation

_* conditions may apply_

<details role="note">
Monad stacks provide an impl for free (*)
(*) conditions may apply
</details>

---

## Chosing the interpreter

<details role="note">
you can fuse everything in a single type for perf reasons
</details>

---

## Going further

<details role="note">
No need to stay constrained to monad transformers
just keep the typeclasses as interface (create your own), provide
the interpreter you want (maybe using monad transformers)
</details>

---

# Tagless final

```haskell



class UserRepo m where
  findUser :: UserId -> m (Maybe User)
  listUsers :: m [User]
```

<details role="note">
you can describe your own behaviours, not just standard ones,
and you can provide different interpreters as well
(prod, debug, mock, …)
</details>

---

## Designing your stack

<details role="note">
Don't try to put every monad you have in the stack or as a class.
</details>

---

## `m (Either e a)` is fine

<details role="note">
`MonadError` is less powerful than having an explicit either
(by design). Put in the stack (or in constraints) what really needs to be common across your application. Trying to fit everything
in the stack will waste your time and over constrain your application

---

## Closing words

<details role="note">
MTL is a powerful style. If your app / HTTP lib is designed around monad stacks, it's a great fit.
Pay attention to the perf (but don't chase perf for the sake of it)
Monad transformers (without MTL) have their uses inside business code (but locally, and shouldn't make it to the signatures)
</details>

---

## Don't panic

<details role="note">
This may seem complex, but
- you can assemble stacks as you please
- it looks complex because we looked at the whole thing
 (think about how it's done in your favourite web framework)
- at least, we have types
</details>
