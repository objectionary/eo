/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Pull a full pipeline.
 *
 * @since 0.31
 */
final class PpPull implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Arrays.<Class<? extends AbstractMojo>>asList(
            MjParse.class,
            MjProbe.class,
            MjPull.class
        ).iterator();
    }
}
