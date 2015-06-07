package util

import java.util.concurrent.atomic.AtomicInteger

/**
 * Created by oliver on 07.06.15.
 */
object Join {
   final class Join(size: Int) {
     private val counter = new AtomicInteger(size)
     private[Join] var kont: () => Unit = () => ()

     @inline
     def andThen(k: => Unit) = {
       kont = () => k
     }

     @inline
     def andThen(k: () => Unit) = {
       kont = k
     }

     @inline
     def join() = counter.incrementAndGet()

     @inline
     def leave() = {
       val i = counter.decrementAndGet()
       if (i == 0)
         kont()
       else if (i < 0)
         throw new IllegalStateException("Join already completed")
     }
   }

   @inline
   def when(j: Join)(k: => Unit) = j andThen k
   def apply(size: Int): Join = new Join(size)
 }
