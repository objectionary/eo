/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import org.eolang.PhCopy;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.slf4j.bridge.SLF4JBridgeHandler;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;

/**
 *
 * Jul To Slf4j Extension
 *
 * @since 0.16.11
 */
public class JulToSlf4jExtension implements BeforeAllCallback {
    private static boolean started = false;

    /**
     * Callback that is invoked once <em>before</em> all tests in the current
     * container.
     *
     * @param context the current extension context; never {@code null}
     */
    @Override
    public void beforeAll(ExtensionContext context) {
        if (!started) {
            started = true;
            context.getRoot().getStore(Namespace.GLOBAL).put("JulToSlf4jExtension", this);
            SLF4JBridgeHandler.install();
            Logger.getLogger("").setLevel(Level.ALL);
        }
    }
}
