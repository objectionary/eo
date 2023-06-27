/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package org.eolang.maven;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import org.junit.jupiter.api.extension.ConditionEvaluationResult;
import org.junit.jupiter.api.extension.ExecutionCondition;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * JUnit's extension to skip a test if we are not online.
 *
 * @since 0.26
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
public final class OnlineCondition implements ExecutionCondition {

    @Override
    public ConditionEvaluationResult evaluateExecutionCondition(
        final ExtensionContext context
    ) {
        ConditionEvaluationResult ret;
        try {
            if (new Online().value()) {
                ret = ConditionEvaluationResult.enabled("We are online!");
            } else {
                ret = ConditionEvaluationResult.disabled("We are offline");
            }
        } catch (final IOException ex) {
            ret = ConditionEvaluationResult.disabled(
                String.format("Failed to check online status: %s", ex.getMessage())
            );
        }
        return ret;
    }

    /**
     * Check if we are online.
     *
     * @since 1.0
     */
    private static class Online {
        /**
         * URL to validate.
         */
        private final String url;

        /**
         * Ctor.
         * @param url URL to check availability for.
         */
        Online(final String url) {
            this.url = url;
        }

        /**
         * Ctor.
         * Check against default url.
         */
        Online() {
            this("https://www.objectionary.com");
        }

        /**
         * If we are online.
         * @return True if we are online and false otherwise.
         * @throws java.io.IOException In case of check failure
         */
        boolean value() throws IOException {
            boolean online = true;
            try {
                final URLConnection conn = new URL(this.url).openConnection();
                conn.connect();
                conn.getInputStream().close();
            } catch (final IOException ignored) {
                online = false;
            }
            return online;
        }
    }
}
