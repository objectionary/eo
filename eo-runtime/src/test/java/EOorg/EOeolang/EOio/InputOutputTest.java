/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
 * @checkstyle PackageNameCheck (20 lines)
 * @checkstyle TrailingCommentCheck (20 lines)
 */
package EOorg.EOeolang.EOio; // NOPMD

import EOorg.EOeolang.EOsys.Posix.CStdLib;
import EOorg.EOeolang.EOsys.Win32.Kernel32;
import EOorg.EOeolang.EOsys.Win32.WinBase;
import EOorg.EOeolang.EOsys.Win32.WinNT;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.AtCompositeTest;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junitpioneer.jupiter.StdIo;

/**
 * All tests for Input/Output operations.
 * They are here in one place because we need to run them in one thread
 * so there are no collisions while reading/writing from/to standard inputs/outputs.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "PMD.TooManyMethods"})
@Execution(ExecutionMode.SAME_THREAD)
final class InputOutputTest {
    /**
     * Console.
     */
    private static final String CONSOLE = "org.eolang.io.console";

    /**
     * Write object.
     */
    private static final String WRITE = "write";

    /**
     * Next line.
     */
    private static final String NEXT_LINE = "next-line";

    @Test
    void dataizesConsoleWriteAsTrue() {
        final Phi phi = new PhWith(
            Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
            "buffer",
            new Data.ToPhi("dataizes as true")
        );
        Assertions.assertTrue(
            new Dataized(phi).asBool(),
            "The `console.write` should have returned TRUE, but it didn't"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesToPosixConsole(@TempDir final Path temp) throws IOException {
        final String msg = "writes to posix console";
        final File file = InputOutputTest.posixStdout(
            temp,
            () -> new Dataized(
                new PhWith(
                    Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
                    "buffer",
                    new Data.ToPhi(msg)
                )
            ).take()
        );
        MatcherAssert.assertThat(
            "The posix 'console.write' should have written to posix console, but it didn't",
            new String(
                Files.readAllBytes(Paths.get(file.getAbsolutePath())), StandardCharsets.UTF_8
            ),
            Matchers.equalTo(msg)
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void writesToWindowsConsole(@TempDir final Path temp) throws IOException {
        final String msg = "writes to windows console";
        final File file = InputOutputTest.windowsStdout(
            temp,
            () -> new Dataized(
                new PhWith(
                    Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
                    "buffer",
                    new Data.ToPhi(msg)
                )
            ).take()
        );
        MatcherAssert.assertThat(
            "The posix 'console.write' should have written to windows console, but it didn't",
            new String(
                Files.readAllBytes(Paths.get(file.getAbsolutePath())), StandardCharsets.UTF_8
            ),
            Matchers.equalTo(msg)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesToConsoleSequentially(@TempDir final Path temp) throws IOException {
        final File file = InputOutputTest.posixStdout(
            temp,
            () -> {
                final Phi buffer = new Data.ToPhi("Ha");
                final Phi first = new PhWith(
                    Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
                    0, buffer
                );
                final Phi second = new PhWith(
                    new PhCopy(
                        new PhMethod(first, InputOutputTest.WRITE)
                    ),
                    0, buffer
                );
                new Dataized(second).take();
            }
        );
        MatcherAssert.assertThat(
            "The posix 'console.write' should have return output block ready to write again, but it didn't",
            new String(
                Files.readAllBytes(Paths.get(file.getAbsolutePath())), StandardCharsets.UTF_8
            ),
            Matchers.equalTo("HaHa")
        );
    }

    @Test
    @StdIo("read via console")
    void readsBytesFromStandardInputViaConsole() {
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read all bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(16)
                )
            ).asString(),
            Matchers.equalTo("read via console")
        );
    }

    @Test
    @StdIo("Message")
    void readsOnlyAvailableBytesFromConsole() {
        MatcherAssert.assertThat(
            String.join(
                "",
                "The object `console.read.read-bytes` should have read only available bytes ",
                "with line separator from standard input, but it didn't"
            ),
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(10)
                )
            ).asString(),
            Matchers.equalTo("Message".concat(System.lineSeparator()))
        );
    }

    @Test
    @StdIo("")
    void readsOnlyNewLineFromEmptyInput() {
        MatcherAssert.assertThat(
            String.join(
                "",
                "The object `console.read.read-bytes` should have returned only line separator ",
                "from empty input, but it didn't"
            ),
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(10)
                )
            ).asString(),
            Matchers.equalTo(System.lineSeparator())
        );
    }

    @Test
    @StdIo("read by portions")
    void readsByPortionsFromInputViaConsole() {
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read first 5 bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(4)
                )
            ).asString(),
            Matchers.equalTo("read")
        );
        MatcherAssert.assertThat(
            "The object `console.read.read-bytes` should have read second 5 bytes from standard input, but it didn't",
            new Dataized(
                new PhWith(
                    new EOconsole$EOread$EOread_bytes(),
                    0, new Data.ToPhi(4)
                )
            ).asString(),
            Matchers.equalTo(" by ")
        );
    }

    @Test
    @StdIo("read sequentially")
    void readsSequentiallyFromInputBlockViaConsole() {
        final Phi console = Phi.Φ.take(InputOutputTest.CONSOLE);
        final Phi first = new PhWith(
            new PhCopy(
                new PhMethod(console, "read")
            ),
            0, new Data.ToPhi(4)
        );
        final Phi second = new PhWith(
            new PhCopy(
                new PhMethod(first, "read")
            ),
            0, new Data.ToPhi(13)
        );
        MatcherAssert.assertThat(
            "The `console.read` object should have return input block ready to `read` again, but it didn't",
            new Dataized(second).asString(),
            Matchers.equalTo(" sequentially")
        );
    }

    @StdIo("this is a test input1!")
    @Test
    void dataizesNextLineOneLine() {
        final String expected = "this is a test input1!";
        final Phi phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(expected)
        );
    }

    @StdIo("this is a testing input2!")
    @Test
    void dataizesStdinOneLine() {
        final String expected = "this is a testing input2!";
        final Phi phi = new PhCopy(new EOstdin());
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(expected)
        );
    }

    @StdIo({"this is a test input3!", "another line", "yet another line"})
    @Test
    void dataizesNextLineMultiLine() {
        final String expected = "this is a test input3!";
        final Phi phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(expected)
        );
    }

    @StdIo("")
    @Test
    void dataizesNextLineEmpty() {
        final Phi phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        final String input = Assertions.assertDoesNotThrow(
            () -> new Dataized(phi).asString(),
            AtCompositeTest.TO_ADD_MESSAGE
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            input,
            Matchers.equalTo("")
        );
    }

    @StdIo("")
    @Test
    void dataizesEmptyStdin() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(new EOstdin()).asString(),
            Matchers.equalTo("")
        );
    }

    @StdIo({"this is a test input4!", "another line", "yet another line"})
    @Test
    void dataizesStdinMultiLine() {
        final String first = "this is a test input4!";
        final String second = "another line";
        final String third = "yet another line";
        final Phi phi = new PhCopy(new EOstdin());
        final String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(String.join(System.lineSeparator(), first, second, third))
        );
    }

    @StdIo({"first", "second", "third"})
    @Test
    void dataizesStdinFewOneLine() {
        final String first = "\u0066\u0069\u0072\u0073\u0074";
        final String second = "\u0073\u0065\u0063\u006F\u006E\u0064";
        final String third = "\u0074\u0068\u0069\u0072\u0064";
        Phi phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(first)
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(second)
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(third)
        );
    }

    @StdIo({"first", "", "third"})
    @Test
    void dataizesStdinEmptyLineBetweenNonEmpty() {
        final String first = "\u0066\u0069\u0072\u0073\u0074";
        final String third = "\u0074\u0068\u0069\u0072\u0064";
        Phi phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        String actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(first)
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo("")
        );
        phi = new PhMethod(new PhCopy(new EOstdin()), InputOutputTest.NEXT_LINE);
        actual = new Dataized(phi).asString();
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo(third)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesPosixWriteSyscallCorrectly() {
        final String msg = "invokes posix write";
        final Phi args = new Data.ToPhi(
            new Phi[] {
                new Data.ToPhi(1L),
                new Data.ToPhi(msg),
                new Data.ToPhi(msg.length()),
            }
        );
        MatcherAssert.assertThat(
            "The \"write\" system call was expected to work correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        Phi.Φ.take("org.eolang.sys.posix").copy(),
                        "name",
                        new Data.ToPhi(InputOutputTest.WRITE)
                    ),
                    "args",
                    args
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(msg.length())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void writesToStdoutViaPosixWriteSyscall(@TempDir final Path temp) throws IOException {
        final String msg = "writes to posix stdout";
        final File file = InputOutputTest.posixStdout(
            temp,
            () -> {
                final Phi args = new Data.ToPhi(
                    new Phi[] {
                        new Data.ToPhi(CStdLib.STDOUT_FILENO),
                        new Data.ToPhi(msg),
                        new Data.ToPhi(msg.length()),
                    }
                );
                new Dataized(
                    new PhWith(
                        new PhWith(
                            Phi.Φ.take("org.eolang.sys.posix").copy(),
                            "name",
                            new Data.ToPhi(InputOutputTest.WRITE)
                        ),
                        "args",
                        args
                    ).take("code")
                );
            }
        );
        MatcherAssert.assertThat(
            "The posix 'write' syscall should have written to standard output, but it didn't",
            new String(
                Files.readAllBytes(Paths.get(file.getAbsolutePath())),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(msg)
        );
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void writesToStdoutViaWindowsFileWriteFunction(@TempDir final Path temp) throws IOException {
        final String msg = "writes to windows stdout";
        final File file = InputOutputTest.windowsStdout(
            temp,
            () -> {
                final Phi args = new Data.ToPhi(
                    new Phi[]{
                        new Data.ToPhi(Kernel32.STD_OUTPUT_HANDLE),
                        new Data.ToPhi(msg),
                        new Data.ToPhi(msg.length()),
                    }
                );
                new Dataized(
                    new PhWith(
                        new PhWith(
                            Phi.Φ.take("org.eolang.sys.win32").copy(),
                            "name",
                            new Data.ToPhi("WriteFile")
                        ),
                        "args",
                        args
                    ).take("code")
                );
            }
        );
        MatcherAssert.assertThat(
            "The win32 'WriteFile' call should have written to standard output, but it didn't",
            new String(
                Files.readAllBytes(Paths.get(file.getAbsolutePath())),
                StandardCharsets.UTF_8
            ),
            Matchers.equalTo(msg)
        );
    }

    /**
     * Redirects Posix stdout.
     * @param temp Temporary directory
     * @param action Action to execute
     * @return Stdout as file
     * @throws IOException If fails to create temporary file
     */
    private static File posixStdout(final Path temp, final Runnable action) throws IOException {
        final File file = Files.createTempFile(
            temp, String.valueOf(action.hashCode()), null
        ).toFile();
        file.deleteOnExit();
        final int descriptor = CStdLib.INSTANCE.open(file.getAbsolutePath(), CStdLib.O_RDWR);
        final int origin = CStdLib.INSTANCE.dup(CStdLib.INSTANCE.STDOUT_FILENO);
        CStdLib.INSTANCE.dup2(descriptor, CStdLib.INSTANCE.STDOUT_FILENO);
        action.run();
        CStdLib.INSTANCE.dup2(origin, CStdLib.INSTANCE.STDOUT_FILENO);
        CStdLib.INSTANCE.close(origin);
        return file;
    }

    /**
     * Redirects Windows stdout.
     * @param temp Temporary directory
     * @param action Action to execute
     * @return Stdout as file
     * @throws IOException If fails to create temporary file
     */
    private static File windowsStdout(final Path temp, final Runnable action) throws IOException {
        final File file = Files.createTempFile(
            temp, String.valueOf(action.hashCode()), null
        ).toFile();
        final WinNT.HANDLE handle = Kernel32.INSTANCE.CreateFile(
            file.getAbsolutePath(),
            Kernel32.GENERIC_WRITE,
            0,
            null,
            Kernel32.CREATE_ALWAYS,
            Kernel32.FILE_ATTRIBUTE_NORMAL,
            null
        );
        assert handle != WinBase.INVALID_HANDLE_VALUE;
        final WinNT.HANDLE origin = Kernel32.INSTANCE.GetStdHandle(Kernel32.STD_OUTPUT_HANDLE);
        Kernel32.INSTANCE.SetStdHandle(Kernel32.STD_OUTPUT_HANDLE, handle);
        action.run();
        Kernel32.INSTANCE.SetStdHandle(Kernel32.STD_OUTPUT_HANDLE, origin);
        Kernel32.INSTANCE.CloseHandle(handle);
        file.deleteOnExit();
        return file;
    }
}
