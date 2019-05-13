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

## Code example for IO

haskell / task

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

## Combining IO steps

<details role="note">
bind (scala / haskell)
</details>

---

## Combining Errors

<details role="note">
bind
</details>

---

## Let's talk DI

<details role="note">
functions
CanIHaz wrapper
Is this a monad?
</details>

---

## Combining DI

---

## Combining things

<details role="note">
monads

functions + laws (haskell / scala)
</details>

---

## Combining effects

<details role="note">
async + DI + http errors
"monads don't compose"
</details>

---

## Monads don't compose

---

## m (n (m (n a))) -> m (n a)

---

## Some monads compose

---

## Examples with IO (Maybe a) and Reader c (IO a)

---

## Monad transformers

---

## Combining effects

<details role="note">
monad stacks
</details>

---

## Example

<details role="note">
ExceptT (ReaderT IO c)
</details>

---

## "Program to an interface"

<details role="note">
Monad stacks are implementation, not interface
</details>

---

## Making things declarative

<details role="note">
MTL-style
</details>

---

## Transformer classes

---

## Biz code example

---

## Chosing the interpreter

<details role="note">
Monad stacks provide an impl for free (*)
(*) conditions may apply
</details>

---

## Chosing the interpreter

<details role="note">
you can fuse everything in a single type for perf reasons
</details>

## Going further

<details role="note">
No need to stay constrained to monad transformers
</details>

## Tagless final

<details role="note">
just keep the typeclasses as interface (create your own), provide
the interpreter you want (maybe using monad transformers)
</details>
