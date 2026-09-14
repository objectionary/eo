/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.HashMap;
import java.util.Map;

/**
 * The data forma a locator names, if it names one at all.
 *
 * <p>It takes a locator, such as {@code Φ.number}. It answers whether that
 * locator is one of the data objects — a number, a string, bytes, a bool
 * or a tuple — and which forma it is. Every other locator answers nothing,
 * whatever the formation behind it may compute.</p>
 *
 * @since 0.77.0
 */
public final class Carrier {

    /**
     * The forma by the locator of each data object.
     */
    private static final Map<String, String> FORMAS = Carrier.formas();

    /**
     * The locator.
     */
    private final String locator;

    /**
     * Ctor.
     *
     * @param place The locator
     */
    public Carrier(final String place) {
        this.locator = place;
    }

    /**
     * Whether the locator names a data object.
     *
     * @return TRUE for a data object
     */
    public boolean data() {
        return Carrier.FORMAS.containsKey(this.locator);
    }

    /**
     * The forma.
     *
     * @return The forma, or the empty string when the locator names no
     *  data object
     */
    String forma() {
        return Carrier.FORMAS.getOrDefault(this.locator, "");
    }

    private static Map<String, String> formas() {
        final Map<String, String> out = new HashMap<>(8);
        out.put("Φ.number", "number");
        out.put("Φ.string", "string");
        out.put("Φ.bytes", "bytes");
        out.put("Φ.bool", "bool");
        out.put("Φ.true", "bool");
        out.put("Φ.false", "bool");
        out.put("Φ.tuple", "tuple");
        out.put("Φ.tuple.empty", "tuple");
        return out;
    }
}
