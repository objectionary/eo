/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOio;

import java.util.Scanner;
import org.eolang.ExFailure;

/**
 * All system inputs.
 *
 * @since 0.27.0
 */
public final class Input {
    /**
     * Input.
     */
    private static Input instance;

    /**
     * Scanner.
     */
    private static Scanner scanner;

    /**
     * Ctor.
     */
    private Input() {
        scanner = new Scanner(System.in);
    }

    /**
     * GetInstance.
     * @return The pointer to input
     */
    public static synchronized Input getInstance() {
        if (instance == null) {
            instance = new Input();
        }
        return instance;
    }

    /**
     * GetLine.
     * @return First read line from system input
     */
    public static String getLine() {
        scanner.useDelimiter(System.lineSeparator());
        final String line = scanner.next();
        scanner.reset();
        return line;
    }

    /**
     * GetAllLines.
     * @return All read lines from system input
     */
    public static String getAllLines() {
        final StringBuilder builder = new StringBuilder();
        while (scanner.hasNextLine()) {
            builder.append(scanner.nextLine()).append(System.lineSeparator());
        }
        scanner.reset();
        return builder.toString();
    }
}
