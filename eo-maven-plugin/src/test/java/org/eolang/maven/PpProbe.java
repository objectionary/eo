/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Probe full pipeline.
 *
 * @since 0.29
 */
final class PpProbe implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Arrays.<Class<? extends AbstractMojo>>asList(
            MjParse.class,
            MjProbe.class
        ).iterator();
    }
}
