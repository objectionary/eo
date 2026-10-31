/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Resolve all eo dependencies.
 *
 * @since 0.29.0
 */
final class PpResolve implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Arrays.<Class<? extends AbstractMojo>>asList(
            MjParse.class,
            MjResolve.class
        ).iterator();
    }
}
