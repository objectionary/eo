/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * The Java of one step that calls back into EO.
 *
 * <p>It takes a {@link Dispatch} or an {@link Entry} and the spelling of
 * the values around it. It answers one Java expression: the operands
 * wrapped back into objects, the method taken of the receiver and applied
 * to the arguments, and the result dataized into the forma the step
 * carries. A call it cannot spell faithfully is refused, and the fragment
 * stays as written.</p>
 *
 * @since 0.76.0
 */
final class Call {

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
    Call(final Step dispatch, final Rendering spelling) {
        this.step = dispatch;
        this.values = spelling;
    }

    /**
     * The Java expression of the call.
     *
     * @return An expression over the locals of the operands
     */
    String text() {
        final List<String> keys = this.step.keys();
        final String atom = this.step.atom();
        final Collection<String> binds = new ArrayList<>(keys.size());
        String call;
        if (atom.charAt(0) == '.') {
            call = String.format(
                "new PhDispatch(%s, \"%s\")", this.receiver(keys.get(0)), atom.substring(1)
            );
            for (int idx = 1; idx < keys.size(); ++idx) {
                binds.add(
                    String.format("new Bind(%d, %s)", idx - 1, this.wrapped(keys.get(idx)))
                );
            }
        } else {
            final String locator = atom.substring(0, atom.indexOf('('));
            if ("formation".equals(this.values.kind(keys.get(0)))) {
                call = this.wrapped(keys.get(0));
            } else {
                call = String.format(
                    "new PhDispatch(%s, \"%s\")",
                    this.receiver(keys.get(0)), locator.substring(locator.lastIndexOf('.') + 1)
                );
            }
            final String[] names = atom.substring(atom.indexOf('(') + 1, atom.length() - 1)
                .split(",", -1);
            for (int idx = 1; idx < keys.size(); ++idx) {
                binds.add(
                    String.format(
                        "new Bind(\"%s\", %s)", names[idx - 1], this.wrapped(keys.get(idx))
                    )
                );
            }
        }
        if (!binds.isEmpty()) {
            call = String.format("new PhApplication(%s, %s)", call, String.join(", ", binds));
        }
        return this.dataized(call);
    }

    private String dataized(final String call) {
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

    private String receiver(final String key) {
        if (this.rebuilt(key)) {
            throw new IllegalStateException(
                String.join(
                    " ",
                    String.format("The receiver '%s' of '%s' is the", key, this.step.atom()),
                    String.format("%s an earlier call was dataized into,", this.values.kind(key)),
                    "and the object it answered with is gone"
                )
            );
        }
        return this.wrapped(key);
    }

    private boolean rebuilt(final String key) {
        boolean out = false;
        if (key.startsWith("sym:s")) {
            final String kind = this.values.kind(key);
            out = !"tuple".equals(kind) && !"object".equals(kind)
                && this.values.step(key.substring(4)).atom().charAt(0) == '.';
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
        } else if ("tuple".equals(kind) || "object".equals(kind) || "formation".equals(kind)) {
            out = this.values.expression(key);
        } else {
            out = String.format("new Data.ToPhi(%s)", this.values.expression(key));
        }
        return out;
    }
}
