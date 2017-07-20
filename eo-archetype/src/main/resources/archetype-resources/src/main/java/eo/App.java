/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package eo;

import java.util.List;

/**
 * Eo entry point.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id $
 * @since 0.1
 */
public final class App implements Runnable {

    /**
     * Eo client.
     */
    private final cli cli;

    /**
     * Ctor.
     *
     * @param args Command line args
     */
    private App(final List<String> args) {
        this.cli = new cli(args);
    }

    @Override
    public void run() {
        this.cli.run();
    }

    /**
     * Java app entry point.
     *
     * @param args Command line args
     */
    @SuppressWarnings(
        {
            "PMD.SystemPrintln",
            "PMD.ProhibitPublicStaticMethods",
            "PMD.UseVarargs"
        }
    )
    public static void main(final String[] args) {
        final Thread thread = new Thread(new App(new Iterable<>(args)));
        thread.start();
        try {
            thread.join();
        } catch (final InterruptedException ex) {
            System.out.println(ex.getLocalizedMessage());
        }
    }
}
