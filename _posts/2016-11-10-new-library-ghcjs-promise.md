---
layout     : post
title      :  "New library: ghcjs-promise"
date       : 2016-10-10 15:34:46
categories : [haskell, javascript]
author     : "Alejandro Durán Pallarés"
comments   : true
---



Hi there, I'd like to introduce you a new library I recently made, [`ghcjs-promise`](https://github.com/vwwv/ghcjs-promise). 

Lately I've using
quiet a lot `ghcjs` to produce javascript from haskell code. Overall, once one get used and set the basic _hello-world_,
it's a pretty satisfactory experience, it allows you to run your haskell code on a web-browser or mobile device
(`react-native`, `ionic`) the same way you'd normally do otherwise. Unfortunately, it's still quiet new, and sometimes, 
not everything you would expect to find on hackage, is there yet, so you can spend a good time coding your own bindings 
to javascript apis.


### Using promises with ghcjs-promise:

Most modern javascript apis, both, web and server/device side, use promises, therefore, a good support for them can simplify
writing haskell bindings to them; I could not find anything related, so I decided to code a library for this very purpose.

The idea is very simple: being able to transform haskell code into a javascript promise, and being able to execute a javascript
promise from haskell. In order to do this, in `ghcjs-promise` you have 2 functions:

- `await`  : takes a promise, and yields its result haskell side. It returns its result inside an `Either`, so you can now 
             whether it produced an exception or not.

  ``` 
  Promise -> IO (Either JSVal JSVal)
  ```

- `promise`: takes a haskell procedure yielding a javascript value, and returns a promise.

   ```
   IO (Either JSVal JSVal) -> IO Promise
   ```

The only new datatype defined is `Promise` and, as you can guess by its name, represents a javascript promise.


Aaaaaaand... that's all, it's indeed a tiny library, so there's no much to say. But let's take a look at [some examples](https://github.com/vwwv/ghcjs-promise-examples). 

#### From Javascript to Haskell

As a first example, let's create a simple binding to the [battery api](https://www.w3.org/TR/battery-status/) and use it to
displayed whether we are using the battery or not. As far as I know, it is only implemented natively on chrome, so this 
example won't work on every browser, we'd use `Maybe` to model that possibility.

First let's define the module header and imports:

{% highlight haskell  %}

{-# LANGUAGE  NoImplicitPrelude 
           ,  DeriveAnyClass
           ,  DeriveGeneric
           ,  JavaScriptFFI
           #-}

import Data.JSVal.Promise
import GHCJS.Marshal 
import GHCJS.Types
import Protolude


{% endhighlight %}


Now, we need to represent the information about the battery, we'd use representation matching the one javascript
side, this way we can automatically derive its parsing.

{% highlight haskell  %}
data BatteryManagery = BatteryManagery 
      { charging        :: Bool
      , chargingTime    :: Double
      , dischargingTime :: Double
      , level           :: Double
      } deriving(Show,Generic,FromJSVal)
{% endhighlight %}

We can now call `navigator.getBattery()`, this js function will return a promise, which in turn, will return the
record we are looking for.

{% highlight haskell  %}
foreign import javascript safe 
   "navigator.getBattery()" js_getBattery :: IO JSVal
{% endhighlight %}

Now we are ready to implement the actual `getBattery` haskell function:

{% highlight haskell  %}
getBattery :: IO (Maybe BatteryManagery)
getBattery = do promise_ <- fromJSVal =<< js_getBattery
                case promise_ of
                   
                 Nothing      -> return Nothing -- not supported :(

                 Just promise -> fromJSVal 
                                  . either identity identity =<< await promise

{% endhighlight %}

Yes! It's done!...umm, ah, well, actually I forgot about the `main` function!

{% highlight haskell  %}
main :: IO ()
main = print =<< getBattery 
{% endhighlight %}

Now it is! You can get the [full code here on github](https://github.com/vwwv/ghcjs-promise-examples/blob/master/src/hs/Battery.hs). 
It was actually quiet easy, not every time we are so lucky, some times for example, 
we need to pass callbacks; in those cases,
getting a nice interface for our binding is not that easy, as callbacks have to be manually garbage-collected... but that's a topic for
another blog post!


#### From Haskell to Javascript

On last example we saw how to use on haskell a promise created javascript wise; that's half the story, let's play now the other way around:
Using on javascript a promise created haskell wise.


Let's imagine an story not that unreal (well, maybe it is). We are making an app to show the user [the meaning of the live](https://www.youtube.com/watch?v=aboZctrHfK8); 
there are 2 development teams, one working on the UI, and the other one working on the "search for the meaning of the live" computation itself.

The first team decide to use javascript, as is normal on web development, and create the following UI :

{% highlight javascript  %}
// on src/js/best_ui_ever.js

function startBestEverUI(compute){
  
  alert( ' Computing the meaning of the live,'
       + ' please take a sit,'
       + ' this gonna take a while'
       );

  compute.then(
    function(meaning){
      alert("The meaning of the live is: "+meaning);
    }
  );
}
{% endhighlight %}

Well, fairly minimalistic, but it will work, now to the haskell part

{% highlight haskell  %}
-- src/hs/MeaningOfLifeApp.hs
{-# LANGUAGE  NoImplicitPrelude 
           ,  JavaScriptFFI
           #-}

import Data.JSVal.Promise
import GHCJS.Marshal 
import GHCJS.Types
import Protolude
{% endhighlight %}


let's compute the meaning of the life:

{% highlight haskell  %}

meaningOfTheLife :: IO Int
meaningOfTheLife = do threadDelay (10*1000000) -- 10 seconds
                      return 42  
{% endhighlight %}


We need to be able to send the computation to the UI, we'll just need to:


1. Add `js-sources: src/js/best_ui_ever.js` to your cabal conf.

2. Foreign import `startBestEverUI` from js, like this: 
   
   {% highlight haskell  %}
   foreign import javascript safe

     "startBestEverUI($1);" connectToUI :: JSVal -> IO ()
   {% endhighlight %}
   

And now that all pieces are implemented, we can finally implement the app:


{% highlight haskell  %}

main :: IO ()
main = connectToUI =<< toJSVal =<< promise computation
  where
    computation =  Right <$> (toJSVal =<< meaningOfTheLife)

{% endhighlight %}


Voilá! You can see the full source code [here](https://github.com/vwwv/ghcjs-promise-examples/blob/master/src/hs/MeaningOfLifeApp.hs) and 
[here](https://github.com/vwwv/ghcjs-promise-examples/blob/master/src/js/best_ui_ever.js).


