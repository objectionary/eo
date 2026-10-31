/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.Iterator;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Printing pipeline.
 *
 * @since 0.33.0
 */
final class PpPrint implements Iterable<Class<? extends AbstractMojo>> {

    @Override
    public Iterator<Class<? extends AbstractMojo>> iterator() {
        return Collections.<Class<? extends AbstractMojo>>singletonList(
            MjPrint.class
        ).iterator();
    }
}
