open Definitions
open Well_formed
open Test_utils

let i_consumer_param = "CONSUMER_PARAM"
let i_consumer =
  (* let rConsumer = *)
  (*   Instance("Consumer", varmap_addvar empty_varmap *)
  (*                                      i_consumer_param *)
  (*                                      (TVar i_consumer_param, Top)) *)
  (* in *)
  Interface ( "Consumer"
            , [i_consumer_param]
            , [], []
            , [ (NoCond, Method( Bot
                               , "accept"
                               , [Arg(TVar i_consumer_param, "t")]))
              ; (NoCond, Method( Instance("Consumer", empty_varmap)
                               , "andThen"
                               , [Arg( Instance("Consumer", empty_varmap)
                                     , "after")]))
              ])

let () =
  let ctx =
    let cc =
      StringMap.add (name_of_inter_t i_consumer) (I i_consumer)
        StringMap.empty
    in
    let sc = StringMap.empty in
    let vm = varmap_addvar empty_varmap
                           i_consumer_param
                           (TVar i_consumer_param, Top)
    in
    context_init cc sc vm
  in
  let () = typecheck (interface_ok ctx i_consumer) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_consumer) in
  ()


(****
/*
 * Copyright (c) 2010, 2013, Oracle and/or its affiliates. All rights reserved.
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
package java.util.function;

import java.util.Objects;

/**
 * Represents an operation that accepts a single input argument and returns no
 * result. Unlike most other functional interfaces, {@code Consumer} is expected
 * to operate via side-effects.
 *
 * <p>This is a <a href="package-summary.html">functional interface</a>
 * whose functional method is {@link #accept(Object)}.
 *
 * @param <T> the type of the input to the operation
 *
 * @since 1.8
 */
@FunctionalInterface
public interface Consumer<T> {

    /**
     * Performs this operation on the given argument.
     *
     * @param t the input argument
     */
    void accept(T t);

    /**
     * Returns a composed {@code Consumer} that performs, in sequence, this
     * operation followed by the {@code after} operation. If performing either
     * operation throws an exception, it is relayed to the caller of the
     * composed operation.  If performing this operation throws an exception,
     * the {@code after} operation will not be performed.
     *
     * @param after the operation to perform after this operation
     * @return a composed {@code Consumer} that performs in sequence this
     * operation followed by the {@code after} operation
     * @throws NullPointerException if {@code after} is null
     */
    default Consumer<T> andThen(Consumer<? super T> after) {
        Objects.requireNonNull(after);
        return (T t) -> { accept(t); after.accept(t); };
    }
}
 ****)
