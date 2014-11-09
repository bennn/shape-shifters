open Definitions
open Well_formed
open Test_utils

let i_iterable_param = "ITERABLE_PARAM"
let i_iterable =
  Interface ( "Iterable"
            , [i_iterable_param]
            , [], []
            , [ (NoCond, Method( Instance("Iterator", empty_varmap)
                               , "iterator"
                               , []))
              ; (NoCond, Method( Bot
                               , "forEach"
                               , [Arg( Instance("Consumer", empty_varmap)
                                     , "action")]))
              ; (NoCond, Method( Instance("Spliterator", empty_varmap)
                               , "spliterator"
                               , []))
              ])

let () =
  let ctx =
    let cc =
      StringMap.add (name_of_inter_t Iterator.i_iterator) (I Iterator.i_iterator)
      (StringMap.add (name_of_inter_t Spliterator.i_spliterator) (I Spliterator.i_spliterator)
      (StringMap.add (name_of_inter_t Consumer.i_consumer) (I Consumer.i_consumer)
        (StringMap.add (name_of_inter_t Boolean.i_boolean) (I Boolean.i_boolean)
          StringMap.empty)))
    in
    let sc = StringMap.empty in
    let vm1 = varmap_addvar empty_varmap
                            Consumer.i_consumer_param
                            (TVar i_iterable_param, Top)
    in
    let vm2 = varmap_addvar vm1
                            Spliterator.i_spliterator_param
                            (Bot, TVar i_iterable_param)
    in
    let vm3 = varmap_addvar vm2
                            Iterator.i_iterator_param
                            (Bot, TVar i_iterable_param)
    in
    let vm4 = varmap_addvar vm3
                            i_iterable_param
                            (Bot, Bot)
    in
    context_init cc sc vm4
  in
  let () = typecheck (interface_ok ctx i_iterable) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_iterable) in
  ()

(***
/*
 * Copyright (c) 2003, 2013, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */
package java.lang;

import java.util.Iterator;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;

/**
 * Implementing this interface allows an object to be the target of
 * the "for-each loop" statement. See
 * <strong>
 * <a href="{@docRoot}/../technotes/guides/language/foreach.html">For-each Loop</a>
 * </strong>
 *
 * @param <T> the type of elements returned by the iterator
 *
 * @since 1.5
 * @jls 14.14.2 The enhanced for statement
 */
public interface Iterable<T> {
    /**
     * Returns an iterator over elements of type {@code T}.
     *
     * @return an Iterator.
     */
    Iterator<T> iterator();

    /**
     * Performs the given action for each element of the {@code Iterable}
     * until all elements have been processed or the action throws an
     * exception.  Unless otherwise specified by the implementing class,
     * actions are performed in the order of iteration (if an iteration order
     * is specified).  Exceptions thrown by the action are relayed to the
     * caller.
     *
     * @implSpec
     * <p>The default implementation behaves as if:
     * <pre>{@code
     *     for (T t : this)
     *         action.accept(t);
     * }</pre>
     *
     * @param action The action to be performed for each element
     * @throws NullPointerException if the specified action is null
     * @since 1.8
     */
    default void forEach(Consumer<? super T> action) {
        Objects.requireNonNull(action);
        for (T t : this) {
            action.accept(t);
        }
    }

    /**
     * Creates a {@link Spliterator} over the elements described by this
     * {@code Iterable}.
     *
     * @implSpec
     * The default implementation creates an
     * <em><a href="Spliterator.html#binding">early-binding</a></em>
     * spliterator from the iterable's {@code Iterator}.  The spliterator
     * inherits the <em>fail-fast</em> properties of the iterable's iterator.
     *
     * @implNote
     * The default implementation should usually be overridden.  The
     * spliterator returned by the default implementation has poor splitting
     * capabilities, is unsized, and does not report any spliterator
     * characteristics. Implementing classes can nearly always provide a
     * better implementation.
     *
     * @return a {@code Spliterator} over the elements described by this
     * {@code Iterable}.
     * @since 1.8
     */
    default Spliterator<T> spliterator() {
        return Spliterators.spliteratorUnknownSize(iterator(), 0);
    }
}
 ***)
