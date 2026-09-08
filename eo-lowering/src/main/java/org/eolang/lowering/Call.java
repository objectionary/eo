/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * The Java of one {@link Dispatch} step: a call back into EO.
 *
 * <p>Every operand is the object it is: a void holds one already and
 * hands it over, read off the atom by the name {@link Rendering} knows
 * it under, while a step and a literal are the datum they are and are
 * wrapped back into an object — a number, a bool and bytes through
 * {@code Data.ToPhi}, a string through the same after the bytes are read
 * as text, since that is what the runtime makes a string of, and a tuple
 * or an object as the {@code Phi} it already is. The method
 * is taken of the receiver with {@code PhDispatch} and applied to the
 * arguments by position with {@code PhApplication}, the way the
 * transpiler spells a call, and the value is dataized into the forma the step
 * carries — a number, a bool, or the bytes of bytes and a string — or
 * left as the object when the forma is {@code object}.</p>
 *
 * @since 0.76.0
 */
public final class Call {

    /**
     * The step to render.
     */
    private final Step step;

    /**
     * The spelling of the values.
     */
    private final Rendering values;

    /**
     * Ctor.
     *
     * @param dispatch The step to render
     * @param spelling The spelling of the values
     */
    public Call(final Step dispatch, final Rendering spelling) {
        this.step = dispatch;
        this.values = spelling;
    }

    /**
     * The Java expression of the call.
     *
     * @return An expression over the locals of the operands
     */
    public String text() {
        final List<String> keys = this.step.keys();
        String call = String.format(
            "new PhDispatch(%s, \"%s\")",
            this.wrapped(keys.get(0)), this.step.atom().substring(1)
        );
        if (keys.size() > 1) {
            final Collection<String> binds = new ArrayList<>(keys.size());
            for (int idx = 1; idx < keys.size(); ++idx) {
                binds.add(
                    String.format("new Bind(%d, %s)", idx - 1, this.wrapped(keys.get(idx)))
                );
            }
            call = String.format("new PhApplication(%s, %s)", call, String.join(", ", binds));
        }
        final String forma = this.step.forma();
        final String out;
        if ("number".equals(forma)) {
            out = String.format("new Dataized(%s).asNumber()", call);
        } else if ("bool".equals(forma)) {
            out = String.format("new Dataized(%s).asBool()", call);
        } else if ("bytes".equals(forma) || "string".equals(forma)) {
            out = String.format("new Dataized(%s).take()", call);
        } else {
            out = call;
        }
        return out;
    }

    private String wrapped(final String key) {
        final String kind = this.values.kind(key);
        final String out;
        if (key.startsWith("sym:v")) {
            out = String.format("this.take(\"%s\")", this.values.named(key));
        } else if ("string".equals(kind)) {
            out = String.format(
                "new Data.ToPhi(new String(%s, java.nio.charset.StandardCharsets.UTF_8))",
                this.values.expression(key)
            );
        } else if ("tuple".equals(kind) || "object".equals(kind)) {
            out = this.values.expression(key);
        } else {
            out = String.format("new Data.ToPhi(%s)", this.values.expression(key));
        }
        return out;
    }
}
