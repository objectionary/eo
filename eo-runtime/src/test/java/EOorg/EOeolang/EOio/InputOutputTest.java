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
import java.util.function.Supplier;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

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
     * Operating system name.
     */
    private static final String OS_NAME = System.getProperty("os.name");

    /**
     * Posix.
     */
    private static final String POSIX = "org.eolang.sys.posix";

    /**
     * Win32.
     */
    private static final String WIN = "org.eolang.sys.win32";

    /**
     * Console.
     */
    private static final String CONSOLE = "org.eolang.io.console";

    /**
     * Stdin.
     */
    private static final String STDIN = "org.eolang.io.stdin";

    /**
     * Write object.
     */
    private static final String WRITE = "write";

    /**
     * Write object.
     */
    private static final String FILE_READ = "FileRead";

    /**
     * Read object.
     */
    private static final String READ = "read";

    /**
     * Next line.
     */
    private static final String NEXT_LINE = "next-line";

    /**
     * Output.
     */
    private static final String OUTPUT = "output";

    /**
     * Redirects stdin.
     * @param temp Temporary directory
     * @param content Stdin content
     * @param action Action to execute
     * @return Read content
     * @throws IOException If fails to create temporary file
     */
    private static byte[] redirectedStdin(
        final Path temp, final String content, final Supplier<byte[]> action
    ) throws IOException {
        final byte[] res;
        if (InputOutputTest.isWindows()) {
            res = InputOutputTest.windowsStdin(temp, content, action);
        } else {
            res = InputOutputTest.posixStdin(temp, content, action);
        }
        return res;
    }

    /**
     * Redirects stdout.
     * @param temp Temporary directory
     * @param action Action to execute
     * @return Stdout as file
     * @throws IOException If fails to create temporary file
     */
    private static File redirectedStdout(final Path temp, final Runnable action)
        throws IOException {
        final File res;
        if (InputOutputTest.isWindows()) {
            res = InputOutputTest.windowsStdout(temp, action);
        } else {
            res = InputOutputTest.posixStdout(temp, action);
        }
        return res;
    }

    /**
     * Redirects Posix stdin.
     * @param temp Temporary directory
     * @param content Content of stdin
     * @param action Action to execute
     * @return Read content
     * @throws IOException If fails to create temporary file
     */
    private static byte[] posixStdin(
        final Path temp, final String content, final Supplier<byte[]> action
    ) throws IOException {
        final File file = Files.createTempFile(
            temp, String.valueOf(action.hashCode()), null
        ).toFile();
        file.deleteOnExit();
        Files.write(file.toPath(), content.getBytes());
        final int descriptor = CStdLib.INSTANCE.open(file.getAbsolutePath(), CStdLib.O_RDWR);
        assert descriptor >= 0;
        final int origin = CStdLib.INSTANCE.dup(CStdLib.INSTANCE.STDIN_FILENO);
        CStdLib.INSTANCE.dup2(descriptor, CStdLib.INSTANCE.STDIN_FILENO);
        final byte[] read = action.get();
        CStdLib.INSTANCE.dup2(origin, CStdLib.INSTANCE.STDIN_FILENO);
        CStdLib.INSTANCE.close(origin);
        CStdLib.INSTANCE.close(descriptor);
        return read;
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
        assert descriptor >= 0;
        final int origin = CStdLib.INSTANCE.dup(CStdLib.INSTANCE.STDOUT_FILENO);
        CStdLib.INSTANCE.dup2(descriptor, CStdLib.INSTANCE.STDOUT_FILENO);
        action.run();
        CStdLib.INSTANCE.dup2(origin, CStdLib.INSTANCE.STDOUT_FILENO);
        CStdLib.INSTANCE.close(origin);
        CStdLib.INSTANCE.close(descriptor);
        return file;
    }

    /**
     * Redirects windows stdin.
     * @param temp Temporary directory
     * @param content Content of stdin
     * @param action Action to run
     * @throws IOException If fails to create temporary file
     */
    private static byte[] windowsStdin(
        final Path temp, final String content, final Supplier<byte[]> action
    ) throws IOException {
        final File file = Files.createTempFile(
            temp, String.valueOf(action.hashCode()), null
        ).toFile();
        file.deleteOnExit();
        Files.write(file.toPath(), content.getBytes(StandardCharsets.UTF_8));
        final WinNT.HANDLE handle = Kernel32.INSTANCE.CreateFile(
            file.getAbsolutePath(),
            Kernel32.GENERIC_READ,
            0,
            null,
            Kernel32.OPEN_EXISTING,
            Kernel32.FILE_ATTRIBUTE_NORMAL,
            null
        );
        assert handle != WinBase.INVALID_HANDLE_VALUE;
        final WinNT.HANDLE origin = Kernel32.INSTANCE.GetStdHandle(Kernel32.STD_INPUT_HANDLE);
        Kernel32.INSTANCE.SetStdHandle(Kernel32.STD_INPUT_HANDLE, handle);
        final byte[] read = action.get();
        Kernel32.INSTANCE.SetStdHandle(Kernel32.STD_INPUT_HANDLE, origin);
        Kernel32.INSTANCE.CloseHandle(handle);
        return read;
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
        file.deleteOnExit();
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
        return file;
    }

    /**
     * Check is current OS is windows.
     * @return Is current OS windows
     */
    private static boolean isWindows() {
        return InputOutputTest.OS_NAME.contains("Windows");
    }

    /**
     * The `console` object test.
     * @since 0.40
     */
    @Nested
    final class ConsoleTest {
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
        void writesToConsole(@TempDir final Path temp) throws IOException {
            final String msg = "writes to posix console";
            final File file = InputOutputTest.redirectedStdout(
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
        void writesToConsoleSequentially(@TempDir final Path temp) throws IOException {
            final File file = InputOutputTest.redirectedStdout(
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
        void readsFromConsole(@TempDir final Path temp) throws IOException {
            final String content = "read from posix console";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    new PhWith(
                        Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.READ).copy(),
                        0,
                        new Data.ToPhi(content.length())
                    )
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'console.read' object should have read all bytes from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        void readsSequentiallyFromInputBlockViaConsole(@TempDir final Path temp)
            throws IOException {
            final String content = "read sequentially from posix console";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> {
                    final Phi console = Phi.Φ.take(InputOutputTest.CONSOLE);
                    final Phi first = new PhWith(
                        new PhCopy(
                            new PhMethod(console, InputOutputTest.READ)
                        ),
                        0, new Data.ToPhi(18)
                    );
                    final Phi second = new PhWith(
                        new PhCopy(
                            new PhMethod(first, InputOutputTest.READ)
                        ),
                        0, new Data.ToPhi(18)
                    );
                    return new Dataized(second).take();
                }
            );
            MatcherAssert.assertThat(
                "The  posix `console.read` object should have return input block ready to `read` again, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo("from posix console")
            );
        }
    }

    /**
     * The `stdin` test.
     * @since 0.40
     */
    @Nested
    final class StdinTest {
        @Test
        void dataizesOneLineOnOneLineInputViaStdin(@TempDir final Path temp) throws IOException {
            final String content = "this is a test input1";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'stdin.next-line' object should have returned one line from one line input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesOneLineOnOneLineWithSeparatorInputViaStdin(
            @TempDir final Path temp
        ) throws IOException {
            final String content = "this is a test input2";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'stdin.next-line' object should have returned one line from one line with separator input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesSecondLineOnThreeLineWithSeparatorInputViaStdin(
            @TempDir final Path temp
        ) throws IOException {
            final String content = String.join(
                System.lineSeparator(),
                "first",
                "second",
                "third"
            );
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> {
                    new Dataized(
                        Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                    ).take();
                    return new Dataized(
                        Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The posix 'stdin.next-line' object should have returned second line from three lines input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo("second")
            );
        }

        @Test
        void dataizesEmptyInputViaStdin(@TempDir final Path temp) throws IOException {
            final String content = "";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'stdin.next-line' object should have returned empty line from empty input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesStdinOneLine(@TempDir final Path temp) throws IOException {
            final String content = "this is a test input3";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    Phi.Φ.take(InputOutputTest.STDIN)
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'stdin' object should have been dataized to one line from one line input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesStdinEmptyLine(@TempDir final Path temp) throws IOException {
            final String content = "";
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    Phi.Φ.take(InputOutputTest.STDIN)
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'stdin' object should have been dataized to one line from one line input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesStdinMultiLine(@TempDir final Path temp) throws IOException {
            final String content = String.join(
                System.lineSeparator(),
                "first",
                "second",
                "third"
            );
            final byte[] result = InputOutputTest.redirectedStdin(
                temp,
                content,
                () -> new Dataized(
                    Phi.Φ.take(InputOutputTest.STDIN)
                ).take()
            );
            MatcherAssert.assertThat(
                "The posix 'stdin' object should have been dataized to one line from one line input",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }
    }

    /**
     * Posix 'read' syscall test.
     * @since 0.40
     */
    @Nested
    final class PosixReadSyscallTest {
        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsFromStdinViaPosixReadSyscall(@TempDir final Path temp) throws IOException {
            final String content = "read from posix stdin";
            final byte[] result = InputOutputTest.posixStdin(
                temp,
                content,
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(CStdLib.STDIN_FILENO),
                            new Data.ToPhi(content.length()),
                        }
                    );
                    return new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                "name",
                                new Data.ToPhi(InputOutputTest.READ)
                            ),
                            "args",
                            args
                        ).take(InputOutputTest.OUTPUT)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read all bytes from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsOnlyAvailableFromStdinViaPosixReadSyscall(@TempDir final Path temp)
            throws IOException {
            final String content = "read available from posix stdin";
            final byte[] result = InputOutputTest.posixStdin(
                temp,
                content,
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(CStdLib.STDIN_FILENO),
                            new Data.ToPhi(content.length() * 2),
                        }
                    );
                    return new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.READ)
                            ),
                            1,
                            args
                        ).take(InputOutputTest.OUTPUT)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read only available bytes from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsFromEmptyStdinViaPosixReadSyscall(@TempDir final Path temp) throws IOException {
            final byte[] result = InputOutputTest.posixStdin(
                temp,
                "",
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(CStdLib.STDIN_FILENO),
                            new Data.ToPhi(10),
                        }
                    );
                    return new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.READ)
                            ),
                            1,
                            args
                        ).take(InputOutputTest.OUTPUT)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read empty string from standard input, but it didn't",
                result.length,
                Matchers.equalTo(0)
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsFromStdinByPortionsViaPosixReadSyscall(@TempDir final Path temp)
            throws IOException {
            final byte[] result = InputOutputTest.posixStdin(
                temp,
                "helloworld",
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(CStdLib.STDIN_FILENO),
                            new Data.ToPhi(5),
                        }
                    );
                    final Phi read = new PhWith(
                        new PhWith(
                            Phi.Φ.take(InputOutputTest.POSIX).copy(),
                            0,
                            new Data.ToPhi(InputOutputTest.READ)
                        ),
                        1,
                        args
                    );
                    read.take(InputOutputTest.OUTPUT);
                    return new Dataized(read.take(InputOutputTest.OUTPUT)).take();
                }
            );
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read empty string from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo("world")
            );
        }
    }

    /**
     * Windows 'FileRead' syscall test.
     * @since 0.40
     */
    @Nested
    final class WindowsFileReadSyscallTest {
        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX, OS.AIX})
        void readsFromStdinViaWindowsFileReadSyscall(@TempDir final Path temp) throws IOException {
            final String content = "read from windows stdin";
            final byte[] result = InputOutputTest.windowsStdin(
                temp,
                content,
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                            new Data.ToPhi(content.length()),
                        }
                    );
                    return new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.WIN).copy(),
                                "name",
                                new Data.ToPhi(InputOutputTest.FILE_READ)
                            ),
                            "args",
                            args
                        ).take(InputOutputTest.OUTPUT)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read all bytes from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX, OS.AIX})
        void readsOnlyAvailableFromStdinViaWindowsFileReadSyscall(@TempDir final Path temp)
            throws IOException {
            final String content = "read available from windows stdin";
            final byte[] result = InputOutputTest.windowsStdin(
                temp,
                content,
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                            new Data.ToPhi(content.length() * 2),
                        }
                    );
                    return new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.WIN).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.FILE_READ)
                            ),
                            1,
                            args
                        ).take(InputOutputTest.OUTPUT)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read only available bytes from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX, OS.AIX})
        void readsFromEmptyStdinViaWindowsFileReadSyscall(@TempDir final Path temp)
            throws IOException {
            final byte[] result = InputOutputTest.windowsStdin(
                temp,
                "",
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                            new Data.ToPhi(10),
                        }
                    );
                    return new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.WIN).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.FILE_READ)
                            ),
                            1,
                            args
                        ).take(InputOutputTest.OUTPUT)
                    ).take();
                }
            );
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read empty string from standard input, but it didn't",
                result.length,
                Matchers.equalTo(0)
            );
        }

        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX, OS.AIX})
        void readsFromStdinByPortionsViaWindowsFileReadSyscall(@TempDir final Path temp)
            throws IOException {
            final byte[] result = InputOutputTest.windowsStdin(
                temp,
                "helloworld",
                () -> {
                    final Phi args = new Data.ToPhi(
                        new Phi[] {
                            new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                            new Data.ToPhi(5),
                        }
                    );
                    final Phi read = new PhWith(
                        new PhWith(
                            Phi.Φ.take(InputOutputTest.WIN).copy(),
                            0,
                            new Data.ToPhi(InputOutputTest.FILE_READ)
                        ),
                        1,
                        args
                    );
                    read.take(InputOutputTest.OUTPUT);
                    return new Dataized(read.take(InputOutputTest.OUTPUT)).take();
                }
            );
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read empty string from standard input, but it didn't",
                new String(result, StandardCharsets.UTF_8),
                Matchers.equalTo("world")
            );
        }
    }

    /**
     * Posix 'write' syscall test.
     * @since 0.40
     */
    @Nested
    final class PosixWriteSyscallTest {
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
                                Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.WRITE)
                            ),
                            1,
                            args
                        ).take("code")
                    ).asNumber();
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
                            Phi.Φ.take(InputOutputTest.POSIX).copy(),
                            0,
                            new Data.ToPhi(InputOutputTest.WRITE)
                        ),
                        1,
                        args
                    ).take("code")
                ).asNumber().intValue(),
                Matchers.equalTo(msg.length())
            );
        }
    }

    /**
     * Windows `FileWrite` syscall test.
     * @since 0.40
     */
    @Nested
    final class WindowsFileWriteSyscallTest {
        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX, OS.AIX})
        void writesToStdoutViaWindowsWriteFileFunction(@TempDir final Path temp)
            throws IOException {
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
                                0,
                                new Data.ToPhi("WriteFile")
                            ),
                            1,
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
    }
}
