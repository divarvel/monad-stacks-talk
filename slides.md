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

![](./assets/mario.jpg)

<details role="note">
today I'll talk about plumbing  
</details>

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

<details role="note">
here, reading env vars is IO  
this info shows up in the types
</details>

---

```scala
// Using cats IO
def putStrLn(s: String): IO[Unit] =
  IO.apply(println(s))

def getHost(): IO[String] = {
  IO.apply(System.getEnv("HOSTNAME"))
}
```

<details role="note">
same in scala, with a bit more ceremony. By default  
the type system does not track IO, we have to use a library  
</details>

---

```scala

def main(): IO[Unit] = for {
  host <- getHost()
  putStrLn(host)
}
```

<details role="note">
same as in haskell, we can pretend we're doing imperative code  
</details>

---

```scala
def runMain(): Unit =
  main().unsafeRunSync
```

<details role="note">
since we have to use a lib, we need to execute the IO  
Hopefully, this should show up only one per codebase.
</details>

---

## Not only IO

<details role="note">
IO is of course a driving force, but it's not the only thing
</details>

---

## Not only IO

<details role="note">
schemas of the handling chain  
 => multiple slides (add observability & DI)
</details>

---

# Modeling Effects

```haskell



-- haskell
forall a. Effect a
```

```scala
// scala
Effect[A]
```

<details role="note">
effects as a context (value + more information)
</details>

---

# Combining IO steps

```haskell



main :: IO ()
main = do
  host <- getHost
  putStrLn host
```

<details role="note">
as seen earlier, there is special syntax for IO
</details>


---

# Combining IO steps

```haskell



main :: IO ()
main = getHost >>= (\host ->
         putStrLn host)
```

<details role="note">
it's just syntactic sugar for regular function calls
</details>

---

# Combining IO steps

```haskell




(>>=) :: IO a -> (a -> IO b) -> IO c
```

<details role="note">
the type shows that the IO has to be sequential  
(else, a would not be available)
</details>

---

# Combining IO steps

```scala



def main(): IO[Unit] = for {
  host <- getHost()
  putStrLn(host)
}
```

<details role="note">
same in scala
</details>

---

# Combining IO steps

```scala



def main(): IO[Unit] = {
  getHost().flatMap(host => {
    putStrLn(host)
  })
```

<details role="note">
syntactic sugar as well
</details>

---

# Combining IO steps

```scala




IO[A]#flatMap(f: A => IO[B]): IO[B]
```

<details role="note">
same type (albeit a bit more noisy)
</details>

---

# Modeling Errors

```haskell



data Either e a =
    Left e
  | Right a

type Effect a = Either Error a
```

<details role="note">
io is not the only thing we can model explicitly in the  
type system. We can replace exceptions with a proper
data structure
</details>

---

# Combining Errors

```haskell



(>>=) :: Either e a
      -> (a -> Either e b)
      -> Either e b
```

<details role="note">
we can combine them the same way as IO. with exceptions,  
each line only runs if the previous ones did not throw
</details>

---

```scala



Either[E,A]
  #flatMap(f: A => Either[E,B])
    : Either[E,B]
```

<details role="note">
same in scala
</details>

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
containers and so on, DI at its core, is simple  

You need something, but it has to be provided
</details>

---

## DI is glorified functions

<details role="note">
I can give you a value if you provide me with x is  
precisely the definition of functions
</details>

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

<details role="note">
newtype is a wrapper (and is erased during compilation)  
This is the same as above, but a bit more expressive
</details>


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

<details role="note">
same as above: you can only have a value if the needed deps  
have been provided
</details>

---

# Combining DI

```haskell



myOperation :: CanIHaz AppConfig MyValue
myOperation = do
  value1 <- myOperation1
  value2 <- myOperation2 value1
  pure (value1 + value2)
```

<details role="note">
no need to juggle with functions any more, everything  
is easy to compose
</details>

---

## Monads describe sequential composition

---

```haskell



forall m a b.
Monad m => m a -> (a -> m b) -> m b
```

<details role="note">
all those functions have the same name and similar signatures  
that's because they're the same thing: sequential composition
</details>

---

```haskell


( $ ) ::   (a ->   b) ->   a ->   b
(<$>) ::   (a ->   b) -> m a -> m b
(<*>) :: m (a ->   b) -> m a -> m b
(=<<) ::   (a -> m b) -> m a -> m b

pure  :: a -> m a
```

<details role="note">
that's the essence of what monads are. we need a few things more
</details>

---

```haskell


( $ ) ::   (a ->   b) -> (  a ->   b)
(<$>) ::   (a ->   b) -> (m a -> m b)
(<*>) :: m (a ->   b) -> (m a -> m b)
(=<<) ::   (a -> m b) -> (m a -> m b)

pure  :: a -> m a
```

<details role="note">
that's the essence of what monads are. we need a few things more
</details>

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

## Combining effectful values <br> (compose monadic values)

---

## Combining effect~~ful~~ ~~value~~s <br> (compose monad~~ic~~ ~~value~~s

---

## Combining effects <br> (compose monads)

<details role="note">
Combining steps separately is nice, but usually I don't have  
only one effect  
async + DI + http errors  

Can I compose them willy-nilly?
</details>

---

## Monads don't compose <br> (in general)

---

```haskell


type Compose m n a = m (n a)

(>>=) :: (Monad m, Monad n)
      => Compose m n a
      -> (a -> Compose m n b)
      -> Compose m n b
```

<details role="note">
this function cannot be written. You can try to,  
to try to get the intuition of why it's not possible.  
The key is to realize when you're in this situation and  
stop trying (I've lost a few hours to this)
</details>

---

## _Some_ monads compose <br> with _all_ monads

<details role="note">
We don't really need to make all monads compose together.  
As long as some monads compose with the rest of them
</details>

---

# Maybe

```haskell



(>>=') :: Monad m
       => m (Maybe a)
       -> (a -> m (Maybe b))
       -> m (Maybe b)
```

<details role="note">
this, you can write (by pattern matching on maybe)
</details>

---

# Either

```haskell



(>>=') :: Monad m
       => m (Either e a)
       -> (a -> m (Either e b))
       -> m (Either e b)
```

<details role="note">
this, you can write (by pattern matching on either)
</details>

---

# CanIHaz (aka Reader)

```haskell



(>>=') :: Monad m
       => Reader e (m a)
       -> (a -> Reader e (m b))
       -> Reader e (m b)
```

<details role="note">
this, you can write (by applying the function)
</details>

---

## Monad transformers

<details role="note">
the key point in all the examples above is that we use specific  
properties of the data types to implement bind.  
things exposed by Monad are not sufficient.  

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

<details role="note">
For technical reasons, we need a wrapper to add new behaviours  
to composition of values. It also makes the code easier to read
</details>

---

```haskell


getSocket :: IO (Maybe String)
getSocket = runMaybeT $ do
  host <- MaybeT (lookupEnv "HOST")
  port <- MaybeT (lookupEnv "PORT")
  pure (host <> ":" <> port)
```

<details role="note">
now we have this composition provided, we can compose steps  
and effects at the same time
</details>


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

<details role="note">
same in scala, we use a wrapper
</details>

---

```scala
def lookupEnv(s: String):
  IO[Maybe[String] =
    // todo

def getSocket(): IO[Maybe[String]] = (
  for {
    host <- OptionT(lookupEnv("HOST"))
    port <- OptionT(lookupEnv("PORT"))
  } yield s"${host}:${port}"
).value
```

<details role="note">
code looks the same as in haskell
</details>

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

<details role="note">
here we combine  
- IO  
- Http level errors  
- Dependency Injection
</details>

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

<details role="note">
runDB requires the DI and IO  
the pattern matching requires http error handling  
I'll talk about how we can write throw error later
</details>

---

# Example

```scala



type Handler[A] =
  ExceptT[Error,IO,A]

type HandlerWithConf[A] =
  ReaderT[Config,Handler,A]
```

<details role="note">
same in scala (with more brackets)
</details>

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

<details role="note">
scala needs a bit more help for pure because of its  
poor type inference, but it's essentially the same
</details>

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
there is another way, called MTL-style that allows  
to declare the _effects_ you need without committing to   
a specific monad stack
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

<details role="note">
here, it's FORALL M, such as M behaves like…  
</details>

---

```haskell



-- provided by a library
runDbTransaction :: Transaction a
                 -> Pool
                 -> IO a
```

<details role="note">
let's say our DB library takes a transaction and runs it  
against a connection pool
</details>


---

```haskell
runDB :: ( Monad m
         , MonadReader Config m
         , MonadIO m)
      => Transaction a
      -> m a

runDB t = do
  pool <- asks dbPool
  liftIO (runDbTransaction t pool)
```

<details role="note">
here we need DI (to get the pool), and IO, to actually  
run the transaction
</details>

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

<details role="note">
for the whole handler, we also need HTTP error handling
</details>

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

<details role="note">
since these are all the effects we talk about,  
we can put them in an alias instead of copying them over and over
</details>

---

# Transformers as a free* implementation

_* conditions may apply_

<details role="note">
Here the code only talks about interfaces. You have to provide  
an implementation that satisfies this interface.  
</details>

---

## Chosing the interpreter

<details role="note">
you can fuse everything in a single type for perf reasons  
the way transformers implement the interface generates a lot of  
calls, this can have a perf cost (it's up to you to decide if  
this is acceptable or not: tradeoffs, tradeoffs)
</details>

---

## Going further

<details role="note">
No need to stay constrained to monad transformers  
just keep the typeclasses as interface (create your own), provide  
the interpreter you want (maybe using monad transformers, maybe not)
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
(by design).

Put in the stack (or in constraints) what really needs to be  
common across your application. Trying to fit everything  
in the stack will waste your time and over constrain  
your application

---

## Closing words

<details role="note">
MTL is a powerful style.  
If your app / HTTP lib is designed around monad stacks,  
it's a great fit.
Pay attention to the perf (but don't chase perf for the sake of it)  
Monad transformers (without MTL) have their uses  
inside business code  
(but locally, and shouldn't make it to the signatures)
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
