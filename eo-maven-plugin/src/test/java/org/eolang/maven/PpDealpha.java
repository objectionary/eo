/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Give the positional arguments of a program the names of their voids.
 *
 * @since 0.69.0
 */
final class PpDealpha implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Arrays.<Class<? extends AbstractMojo>>asList(
            MjParse.class,
            MjInference.class,
            MjDealpha.class
        ).iterator();
    }
}
