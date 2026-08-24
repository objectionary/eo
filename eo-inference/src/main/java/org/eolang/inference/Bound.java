/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * What every application puts into the voids of what it copies.
 *
 * <p>An application is the one place in a program where a void stops being a
 * question: {@code inc t} says that the {@code x} of that {@code inc} holds a
 * {@code t}. The place of an argument says which void it lands in, counted
 * through the voids in the order the formation declares them, which is the
 * order {@link Ungrouped} keeps.</p>
 *
 * <p>What fills the void is written down as the argument itself and not as
 * what the argument turns out to be. The two differ exactly when the argument
 * is an application of its own: {@code inc (dec u)} fills the {@code x} of the
 * {@code inc} with an object that has filled the {@code x} of a {@code dec},
 * and naming {@code Φ.dec} here would throw that half away. The argument has a
 * row of its own, which says both.</p>
 *
 * <p>The receiver of a dispatch fills a void too, when the formation it
 * dispatches into declares one for it. That void is named {@code ρ} and is
 * not counted among the places, since it is answered by whoever dispatches
 * and not by an argument written to the right of the name.</p>
 *
 * @since 0.69.0
 */
final class Bound {

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * What every dispatch takes its attribute from, from {@link Xmirs}.
     */
    private final Map<String, String> receivers;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param arguments The arguments of every application, from {@link Given}
     * @param taken What every dispatch takes its attribute from
     * @param aliases The name every type goes by, from {@link Ends}
     * @param provided What the types certainly have
     */
    Bound(
        final Map<String, List<String>> arguments,
        final Map<String, String> taken,
        final Map<String, String> aliases,
        final Provided provided
    ) {
        this.args = arguments;
        this.receivers = taken;
        this.names = aliases;
        this.owned = provided;
    }

    /**
     * What every application fills, by the locator of the application.
     * @return The objects the voids hold, by the locator of the void, in the
     *  order the voids were declared, without the applications that fill
     *  nothing we can name
     */
    Map<String, Map<String, String>> all() {
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, List<String>> application : this.args.entrySet()) {
            final Map<String, String> filled = this.filled(
                this.names.getOrDefault(application.getKey(), application.getKey()),
                application.getValue()
            );
            if (!filled.isEmpty()) {
                found.put(application.getKey(), filled);
            }
        }
        for (final Map.Entry<String, String> dispatch : this.receivers.entrySet()) {
            final String hollow = this.owned.receiver(
                this.names.getOrDefault(dispatch.getKey(), dispatch.getKey())
            );
            if (!hollow.isEmpty()) {
                found.computeIfAbsent(dispatch.getKey(), key -> new LinkedHashMap<>(1))
                    .put(hollow, dispatch.getValue());
            }
        }
        return found;
    }

    private Map<String, String> filled(final String copied, final List<String> given) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (int place = 0; place < given.size(); place += 1) {
            final String hollow = this.owned.slot(copied, place);
            if (!hollow.isEmpty() && !given.get(place).isEmpty()) {
                found.put(hollow, given.get(place));
            }
        }
        return found;
    }
}
