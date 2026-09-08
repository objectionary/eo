/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * The chain of causes of a problem, ready for the log.
 *
 * <p>The chain is flattened, and a cause that merely repeats a part of an
 * earlier one is dropped, so that a reader of the log sees every reason
 * once and nothing else.</p>
 *
 * @since 0.62.0
 */
final class Causes implements Iterable<String> {

    /**
     * The problem to explain.
     */
    private final Throwable problem;

    /**
     * Ctor.
     *
     * @param problem The problem to explain
     */
    Causes(final Throwable problem) {
        this.problem = problem;
    }

    @Override
    public Iterator<String> iterator() {
        final List<String> causes = Causes.all(this.problem);
        for (int pos = 0; pos < causes.size(); ++pos) {
            if (causes.get(pos) == null) {
                causes.remove(pos);
                break;
            }
        }
        int idx = 0;
        while (idx < causes.size()) {
            final String cause = causes.get(idx);
            for (int later = idx + 1; later < causes.size(); ++later) {
                final String another = causes.get(later);
                if (another != null && cause.contains(another)) {
                    causes.remove(idx);
                    idx -= 1;
                    break;
                }
            }
            idx += 1;
        }
        return new LinkedHashSet<>(causes).iterator();
    }

    private static List<String> all(final Throwable problem) {
        final List<String> causes = new ArrayList<>(0);
        causes.add(problem.getMessage());
        final Throwable cause = problem.getCause();
        if (cause != null) {
            causes.addAll(Causes.all(cause));
        }
        return causes;
    }
}
