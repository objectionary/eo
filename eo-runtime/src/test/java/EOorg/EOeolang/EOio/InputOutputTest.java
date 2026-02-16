/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (20 lines)
 * @checkstyle TrailingCommentCheck (20 lines)
 */
package EOorg.EOeolang.EOio; // NOPMD

import EOorg.EOeolang.EOsm.Posix.CStdLib;
import EOorg.EOeolang.EOsm.Win32.Kernel32;
import EOorg.EOeolang.EOsm.Win32.WinBase;
import EOorg.EOeolang.EOsm.Win32.WinNT;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
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
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * All tests for Input/Output operations.
 * They are here in one place because we need to run them in one thread
 * so there are no collisions while reading/writing from/to standard inputs/outputs.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@Execution(ExecutionMode.SAME_THREAD)
@ExtendWith(MktmpResolver.class)
final class InputOutputTest {
    /**
     * Operating system name.
     */
    private static final String OS_NAME = System.getProperty("os.name");

    /**
     * Posix.
     */
    private static final String POSIX = "org.eolang.sm.posix";

    /**
     * Win32.
     */
    private static final String WIN = "org.eolang.sm.win32";

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
    private static final String READ_FILE = "ReadFile";

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
        Files.write(file.toPath(), content.getBytes(StandardCharsets.UTF_8));
        final int descriptor = CStdLib.INSTANCE.open(file.getAbsolutePath(), CStdLib.O_RDONLY);
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
    @Execution(ExecutionMode.SAME_THREAD)
    final class ConsoleTest {
        @Test
        void dataizesConsoleWriteAsTrue() {
            Assertions.assertTrue(
                new Dataized(
                    new PhWith(
                        Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
                        "buffer",
                        new Data.ToPhi("dataizes as true")
                    )
                ).asBool(),
                "The `console.write` should have returned TRUE, but it didn't"
            );
        }

        @Test
        void writesToConsole(@Mktmp final Path temp) throws IOException {
            MatcherAssert.assertThat(
                "The 'console.write' should have written to console, but it didn't",
                Files.readString(
                    Paths.get(
                        InputOutputTest.redirectedStdout(
                            temp,
                            () -> new Dataized(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
                                    "buffer",
                                    new Data.ToPhi("writes to console")
                                )
                            ).take()
                        ).getAbsolutePath()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.containsString("writes to console")
            );
        }

        @Test
        void writesToConsoleSequentially(@Mktmp final Path temp) throws IOException {
            MatcherAssert.assertThat(
                "The 'console.write' should have return output block ready to write again, but it didn't",
                Files.readString(
                    Paths.get(
                        InputOutputTest.redirectedStdout(
                            temp,
                            () -> new Dataized(
                                new PhWith(
                                    new PhCopy(
                                        new PhMethod(
                                            new PhWith(
                                                Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.WRITE).copy(),
                                                0, new Data.ToPhi("Hey")
                                            ),
                                            InputOutputTest.WRITE
                                        )
                                    ),
                                    0, new Data.ToPhi("There")
                                )
                            ).take()
                        ).getAbsolutePath()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.allOf(
                    Matchers.containsString("Hey"),
                    Matchers.containsString("There")
                )
            );
        }

        @Test
        void readsFromConsole(@Mktmp final Path temp) throws IOException {
            final String content = "read from console";
            MatcherAssert.assertThat(
                "The 'console.read' object should have read all bytes from standard input, but it didn't",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.CONSOLE).take(InputOutputTest.READ).copy(),
                                0,
                                new Data.ToPhi(content.length())
                            )
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        void readsSequentiallyFromInputBlockViaConsole(@Mktmp final Path temp)
            throws IOException {
            MatcherAssert.assertThat(
                "The `console.read` object should have return input block ready to `read` again, but it didn't",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        "read sequentially from console",
                        () -> new Dataized(
                            new PhWith(
                                new PhCopy(
                                    new PhMethod(
                                        new PhWith(
                                            new PhCopy(
                                                new PhMethod(
                                                    Phi.Φ.take(InputOutputTest.CONSOLE),
                                                    InputOutputTest.READ
                                                )
                                            ),
                                            0, new Data.ToPhi(18)
                                        ),
                                        InputOutputTest.READ
                                    )
                                ),
                                0, new Data.ToPhi(18)
                            )
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("from console")
            );
        }
    }

    /**
     * The `stdin` test.
     * @since 0.40
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    final class StdinTest {
        @Test
        void dataizesOneLineOnOneLineInputViaStdin(@Mktmp final Path temp) throws IOException {
            final String content = "this is a test input1";
            MatcherAssert.assertThat(
                "The 'stdin.next-line' object should have returned one line from one line input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesOneLineOnOneLineWithSeparatorInputViaStdin(
            @Mktmp final Path temp
        ) throws IOException {
            final String content = "this is a test input2";
            MatcherAssert.assertThat(
                "The 'stdin.next-line' object should have returned one line from one line with separator input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesSecondLineOnThreeLineWithSeparatorInputViaStdin(
            @Mktmp final Path temp
        ) throws IOException {
            MatcherAssert.assertThat(
                "The 'stdin.next-line' object should have returned second line from three lines input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        String.join(
                            System.lineSeparator(),
                            "first",
                            "second",
                            "third"
                        ),
                        () -> {
                            new Dataized(
                                Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                            ).take();
                            return new Dataized(
                                Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                            ).take();
                        }
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("second")
            );
        }

        @Test
        void dataizesEmptyInputViaStdin(@Mktmp final Path temp) throws IOException {
            MatcherAssert.assertThat(
                "The 'stdin.next-line' object should have returned empty line from empty input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        "",
                        () -> new Dataized(
                            Phi.Φ.take(InputOutputTest.STDIN).take(InputOutputTest.NEXT_LINE)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("")
            );
        }

        @Test
        void dataizesStdinOneLine(@Mktmp final Path temp) throws IOException {
            final String content = "this is a test input3";
            MatcherAssert.assertThat(
                "The 'stdin' object should have been dataized to one line from one line input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            Phi.Φ.take(InputOutputTest.STDIN)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        void dataizesStdinEmptyLine(@Mktmp final Path temp) throws IOException {
            MatcherAssert.assertThat(
                "The 'stdin' object should have been dataized to one line from one line input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        "",
                        () -> new Dataized(
                            Phi.Φ.take(InputOutputTest.STDIN)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("")
            );
        }

        @Test
        void dataizesStdinMultiLine(@Mktmp final Path temp) throws IOException {
            final String content = String.join(
                System.lineSeparator(),
                "first",
                "second",
                "third"
            );
            MatcherAssert.assertThat(
                "The 'stdin' object should have been dataized to one line from one line input",
                new String(
                    InputOutputTest.redirectedStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            Phi.Φ.take(InputOutputTest.STDIN)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }
    }

    /**
     * Posix 'read' syscall test.
     * @since 0.40
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    final class PosixReadSyscallTest {
        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsFromStdinViaPosixReadSyscall(@Mktmp final Path temp) throws IOException {
            final String content = "read from posix stdin";
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read all bytes from standard input, but it didn't",
                new String(
                    InputOutputTest.posixStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            new PhWith(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                    "name",
                                    new Data.ToPhi(InputOutputTest.READ)
                                ),
                                "args",
                                new Data.ToPhi(
                                    new Phi[] {
                                        new Data.ToPhi(CStdLib.STDIN_FILENO),
                                        new Data.ToPhi(content.length()),
                                    }
                                )
                            ).take(InputOutputTest.OUTPUT)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsOnlyAvailableFromStdinViaPosixReadSyscall(@Mktmp final Path temp)
            throws IOException {
            final String content = "read available from posix stdin";
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read only available bytes from standard input, but it didn't",
                new String(
                    InputOutputTest.posixStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            new PhWith(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                    0,
                                    new Data.ToPhi(InputOutputTest.READ)
                                ),
                                1,
                                new Data.ToPhi(
                                    new Phi[] {
                                        new Data.ToPhi(CStdLib.STDIN_FILENO),
                                        new Data.ToPhi(content.length() * 2),
                                    }
                                )
                            ).take(InputOutputTest.OUTPUT)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsFromEmptyStdinViaPosixReadSyscall(@Mktmp final Path temp) throws IOException {
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read empty string from standard input, but it didn't",
                InputOutputTest.posixStdin(
                    temp,
                    "",
                    () -> new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.READ)
                            ),
                            1,
                            new Data.ToPhi(
                                new Phi[] {
                                    new Data.ToPhi(CStdLib.STDIN_FILENO),
                                    new Data.ToPhi(10),
                                }
                            )
                        ).take(InputOutputTest.OUTPUT)
                    ).take()
                ).length,
                Matchers.equalTo(0)
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void readsFromStdinByPortionsViaPosixReadSyscall(@Mktmp final Path temp)
            throws IOException {
            MatcherAssert.assertThat(
                "The posix 'read' syscall should have read empty string from standard input, but it didn't",
                new String(
                    InputOutputTest.posixStdin(
                        temp,
                        "helloworld",
                        () -> {
                            final Phi read = new PhWith(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                    0,
                                    new Data.ToPhi(InputOutputTest.READ)
                                ),
                                1,
                                new Data.ToPhi(
                                    new Phi[] {
                                        new Data.ToPhi(CStdLib.STDIN_FILENO),
                                        new Data.ToPhi(5),
                                    }
                                )
                            );
                            read.take(InputOutputTest.OUTPUT);
                            return new Dataized(read.take(InputOutputTest.OUTPUT)).take();
                        }
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("world")
            );
        }
    }

    /**
     * Windows 'FileRead' syscall test.
     * @since 0.40
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    final class WindowsFileReadSyscallTest {
        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX})
        void readsFromStdinViaWindowsFileReadSyscall(@Mktmp final Path temp) throws IOException {
            final String content = "read from windows stdin";
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read all bytes from standard input, but it didn't",
                new String(
                    InputOutputTest.windowsStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            new PhWith(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.WIN).copy(),
                                    "name",
                                    new Data.ToPhi(InputOutputTest.READ_FILE)
                                ),
                                "args",
                                new Data.ToPhi(
                                    new Phi[] {
                                        new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                                        new Data.ToPhi(content.length()),
                                    }
                                )
                            ).take(InputOutputTest.OUTPUT)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX})
        void readsOnlyAvailableFromStdinViaWindowsFileReadSyscall(@Mktmp final Path temp)
            throws IOException {
            final String content = "read available from windows stdin";
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read only available bytes from standard input, but it didn't",
                new String(
                    InputOutputTest.windowsStdin(
                        temp,
                        content,
                        () -> new Dataized(
                            new PhWith(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.WIN).copy(),
                                    0,
                                    new Data.ToPhi(InputOutputTest.READ_FILE)
                                ),
                                1,
                                new Data.ToPhi(
                                    new Phi[] {
                                        new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                                        new Data.ToPhi(content.length() * 2),
                                    }
                                )
                            ).take(InputOutputTest.OUTPUT)
                        ).take()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo(content)
            );
        }

        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX})
        void readsFromEmptyStdinViaWindowsFileReadSyscall(@Mktmp final Path temp)
            throws IOException {
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read empty string from standard input, but it didn't",
                InputOutputTest.windowsStdin(
                    temp,
                    "",
                    () -> new Dataized(
                        new PhWith(
                            new PhWith(
                                Phi.Φ.take(InputOutputTest.WIN).copy(),
                                0,
                                new Data.ToPhi(InputOutputTest.READ_FILE)
                            ),
                            1,
                            new Data.ToPhi(
                                new Phi[] {
                                    new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                                    new Data.ToPhi(10),
                                }
                            )
                        ).take(InputOutputTest.OUTPUT)
                    ).take()
                ).length,
                Matchers.equalTo(0)
            );
        }

        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX})
        void readsFromStdinByPortionsViaWindowsFileReadSyscall(@Mktmp final Path temp)
            throws IOException {
            MatcherAssert.assertThat(
                "The windows 'read' syscall should have read empty string from standard input, but it didn't",
                new String(
                    InputOutputTest.windowsStdin(
                        temp,
                        "helloworld",
                        () -> {
                            final Phi read = new PhWith(
                                new PhWith(
                                    Phi.Φ.take(InputOutputTest.WIN).copy(),
                                    0,
                                    new Data.ToPhi(InputOutputTest.READ_FILE)
                                ),
                                1,
                                new Data.ToPhi(
                                    new Phi[] {
                                        new Data.ToPhi(Kernel32.STD_INPUT_HANDLE),
                                        new Data.ToPhi(5),
                                    }
                                )
                            );
                            read.take(InputOutputTest.OUTPUT);
                            return new Dataized(read.take(InputOutputTest.OUTPUT)).take();
                        }
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("world")
            );
        }
    }

    /**
     * Posix 'write' syscall test.
     * @since 0.40
     */
    @Nested
    @Execution(ExecutionMode.SAME_THREAD)
    final class PosixWriteSyscallTest {
        @Test
        @DisabledOnOs(OS.WINDOWS)
        void writesToStdoutViaPosixWriteSyscall(@Mktmp final Path temp) throws IOException {
            MatcherAssert.assertThat(
                "The posix 'write' syscall should have written to standard output, but it didn't",
                Files.readString(
                    Paths.get(
                        InputOutputTest.posixStdout(
                            temp,
                            () -> new Dataized(
                                new PhWith(
                                    new PhWith(
                                        Phi.Φ.take(InputOutputTest.POSIX).copy(),
                                        0,
                                        new Data.ToPhi(InputOutputTest.WRITE)
                                    ),
                                    1,
                                    new Data.ToPhi(
                                        new Phi[] {
                                            new Data.ToPhi(CStdLib.STDOUT_FILENO),
                                            new Data.ToPhi("writes to posix stdout"),
                                            new Data.ToPhi("writes to posix stdout".length()),
                                        }
                                    )
                                ).take("code")
                            ).asNumber()
                        ).getAbsolutePath()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.containsString("writes to posix stdout")
            );
        }

        @Test
        @DisabledOnOs(OS.WINDOWS)
        void invokesPosixWriteSyscallCorrectly() {
            final String msg = "invokes posix write";
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
                        new Data.ToPhi(
                            new Phi[] {
                                new Data.ToPhi(1L),
                                new Data.ToPhi(msg),
                                new Data.ToPhi(msg.length()),
                            }
                        )
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
    @Execution(ExecutionMode.SAME_THREAD)
    final class WindowsFileWriteSyscallTest {
        @Test
        @DisabledOnOs({OS.MAC, OS.LINUX})
        void writesToStdoutViaWindowsWriteFileFunction(@Mktmp final Path temp)
            throws IOException {
            MatcherAssert.assertThat(
                "The win32 'WriteFile' call should have written to standard output, but it didn't",
                Files.readString(
                    Paths.get(
                        InputOutputTest.windowsStdout(
                            temp,
                            () -> new Dataized(
                                new PhWith(
                                    new PhWith(
                                        Phi.Φ.take("org.eolang.sm.win32").copy(),
                                        0,
                                        new Data.ToPhi("WriteFile")
                                    ),
                                    1,
                                    new Data.ToPhi(
                                        new Phi[]{
                                            new Data.ToPhi(Kernel32.STD_OUTPUT_HANDLE),
                                            new Data.ToPhi("writes to windows stdout"),
                                            new Data.ToPhi("writes to windows stdout".length()),
                                        }
                                    )
                                ).take("code")
                            )
                        ).getAbsolutePath()
                    ),
                    StandardCharsets.UTF_8
                ),
                Matchers.equalTo("writes to windows stdout")
            );
        }
    }
}
