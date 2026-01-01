/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolver;

/**
 * This class is instantiated and then called by JUnit when
 * an argument of a test method is marked with the {@link RandomProgram}
 * annotation.
 *
 * @since 0.42.0
 */
public final class RandomProgramResolver implements ParameterResolver {

    @Override
    public boolean supportsParameter(final ParameterContext context,
        final ExtensionContext ext) {
        return context.getParameter().getType().equals(String.class)
            && context.isAnnotated(RandomProgram.class);
    }

    @Override
    public Object resolveParameter(final ParameterContext context, final ExtensionContext ext) {
        return String.join(
            "\n",
            "# This is a random program in EO, which supposedly",
            "# complies with all syntactic rules of the language,",
            "# include the requirements for comments.",
            "[] > foo",
            "  QQ.io.stdout > @",
            "    \"Hello, world!\\n\"",
            ""
        );
    }

}
