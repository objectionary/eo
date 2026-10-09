/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.regex.Pattern;

/**
 * The entries that phino is allowed to run on, chosen by their locators.
 *
 * <p>A scope is made of two regular expressions. The first one says which
 * entries are included, and the second one says which of them are
 * excluded again. Each of them must match the whole locator, such as
 * {@code Φ.string.printf}, and not only a part of it. This is needed when
 * one slow entry has to be studied alone, or when one entry has to be
 * kept away from phino, while all the others are lowered as usual.</p>
 *
 * @since 0.64.0
 */
public final class Scope {

    /**
     * The regular expression of the included entries.
     */
    private final Pattern included;

    /**
     * The regular expression of the excluded entries.
     */
    private final Pattern excluded;

    /**
     * Ctor.
     *
     * @param inc The regular expression of the included entries
     * @param exc The regular expression of the excluded entries
     */
    public Scope(final String inc, final String exc) {
        this(Pattern.compile(inc), Pattern.compile(exc));
    }

    /**
     * Ctor.
     *
     * @param inc The regular expression of the included entries
     * @param exc The regular expression of the excluded entries
     */
    public Scope(final Pattern inc, final Pattern exc) {
        this.included = inc;
        this.excluded = exc;
    }

    /**
     * Whether phino may run on the entry with this locator.
     *
     * @param locator The locator of the entry, such as {@code Φ.string.printf}
     * @return TRUE if the entry is included and not excluded
     */
    public boolean covers(final String locator) {
        return this.included.matcher(locator).matches()
            && !this.excluded.matcher(locator).matches();
    }

    @Override
    public String toString() {
        return String.format("'%s' but not '%s'", this.included, this.excluded);
    }
}
