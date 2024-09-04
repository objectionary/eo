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
package EOorg.EOeolang.EOsys.Win32; // NOPMD

import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

/**
 * Interface definitions for <code>kernel32.dll</code>.
 * @since 0.40
 */
@SuppressWarnings("PMD.MethodNamingConventions")
public interface Kernel32 extends StdCallLibrary, WinNT, Wincon {
    /**
     * Instance.
     */
    Kernel32 INSTANCE = Native.load("Kernel32", Kernel32.class, W32APIOptions.DEFAULT_OPTIONS);

    /**
     * Get a handle to specified standard device.
     * @param handle The standard device identifier
     * @return A handle to the specified standard device (standard input, output, or error)
     * @checkstyle MethodNameCheck (5 lines)
     */
    HANDLE GetStdHandle(int handle);

    /**
     * Set a standard device handle.
     * @param std The standard device identifier
     * @param handle The handle for the standard device
     * @return True if successful, if False then use {@code GetLastError()} to get extended error
     *  information
     * @see <a href="https://msdn.microsoft.com/en-us/library/ms686244(v=vs.85).aspx">SetStdHandle
     *  documentation</a>
     * @checkstyle MethodNameCheck (5 lines)
     */
    boolean SetStdHandle(int std, HANDLE handle);

    /**
     * Closes an open object handle.
     * @param handle Handle to an open object. This parameter can be a pseudo handle or
     *  INVALID_HANDLE_VALUE.
     * @return If the function succeeds, the return value is nonzero. If the function fails, the
     *  return value is zero. To get extended error information, call {@code GetLastError}.
     * @see <A HREF="https://msdn.microsoft.com/en-us/library/windows/desktop/ms724211(v=vs.85).aspx">CloseHandle</A>
     * @checkstyle MethodNameCheck (5 lines)
     */
    boolean CloseHandle(HANDLE handle);

    /**
     * Writes data to the specified file or input/output (I/O) device.
     * @param handle A handle to the file or I/O device (for example, a file, file stream, physical
     *  disk, volume, console buffer, tape drive, socket, communications resource, mailslot, or
     *  pipe).
     * @param buffer A pointer to the buffer containing the data to be written to the file or
     *  device.
     * @param count The number of bytes to be written to the file or device.
     * @param written A pointer to the variable that receives the number of bytes written when using
     *  a synchronous hFile parameter.
     * @param overlapped A pointer to an {@link OVERLAPPED} structure is required if the hFile
     *  parameter was opened with FILE_FLAG_OVERLAPPED, otherwise this parameter can be NULL.
     * @return If the function succeeds, the return value is nonzero (TRUE). If the function fails,
     *  or is completing asynchronously, the return value is zero (FALSE). To get extended error
     *  information, call the GetLastError function.
     * @checkstyle MethodNameCheck (5 lines)
     * @checkstyle ParameterNumberCheck (20 lines)
     */
    boolean WriteFile(
        HANDLE handle,
        byte[] buffer,
        int count,
        IntByReference written,
        OVERLAPPED overlapped
    );

    /**
     * Reads data from the specified file or input/output (I/O) device. Reads
     * occur at the position specified by the file pointer if supported by the
     * device.
     * @param handle A handle to the device (for example, a file, file stream,
     *  physical disk, volume, console buffer, tape drive, socket,
     *  communications resource, mailslot, or pipe).
     * @param buffer A pointer to the buffer that receives the data read from a
     *  file or device.
     * @param count The maximum number of bytes to be read.
     * @param read A pointer to the variable that receives the number of bytes
     *  read when using a synchronous hFile parameter
     * @param overlapped A pointer to an OVERLAPPED structure is required if the handle
     *  parameter was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.
     * @return If the function succeeds, the return value is nonzero (TRUE). If
     *  the function fails, or is completing asynchronously, the return
     *  value is zero (FALSE).
     * @checkstyle MethodNameCheck (5 lines)
     * @checkstyle ParameterNumberCheck (20 lines)
     */
    boolean ReadFile(
        HANDLE handle,
        byte[] buffer,
        int count,
        IntByReference read,
        WinBase.OVERLAPPED overlapped
    );

    /**
     * The CreateFile function creates or opens a file, file stream, directory, physical disk,
     * volume, console buffer, tape drive, communications resource, mailslot, or named pipe. The
     * function returns a handle that can be used to access an object.
     * @param name A pointer to a null-terminated string that specifies the name of an object to
     *  create or open.
     * @param access The access to the object, which can be read, write, or both.
     * @param mode The sharing mode of an object, which can be read, write, both, or none.
     * @param security A pointer to a SECURITY_ATTRIBUTES structure that determines whether the
     *  returned handle can be inherited by child processes. If lpSecurityAttributes is NULL, the
     *  handle cannot be inherited.
     * @param disposition An action to take on files that exist and do not exist.
     * @param flags The file attributes and flags.
     * @param template Handle to a template file with the GENERIC_READ access right. The template
     *  file supplies file attributes and extended attributes for the file that is being created.
     *  This parameter can be NULL.
     * @return If the function succeeds, the return value is an open handle to a specified file. If
     *  a specified file exists before the function call and dwCreationDisposition is CREATE_ALWAYS
     *  or OPEN_ALWAYS, a call to GetLastError returns ERROR_ALREADY_EXISTS, even when the function
     *  succeeds. If a file does not exist before the call, GetLastError returns 0 (zero). If the
     *  function fails, the return value is INVALID_HANDLE_VALUE. To get extended error information,
     *  call GetLastError.
     * @checkstyle MethodNameCheck (5 lines)
     * @checkstyle ParameterNumberCheck (20 lines)
     */
    HANDLE CreateFile(
        String name,
        int access,
        int mode,
        SECURITY_ATTRIBUTES security,
        int disposition,
        int flags,
        HANDLE template
    );

    /**
     * Retrieves the contents of the specified variable from the environment
     * block of the calling process.
     * @param name The name of the environment variable.
     * @param buffer A pointer to a buffer that receives the contents of the
     *  specified environment variable as a null-terminated string. An
     *  environment variable has a maximum size limit of 32,767
     *  characters, including the null-terminating character.
     * @param size The size of the buffer pointed to by the buffer parameter,
     *  including the null-terminating character, in characters.
     * @return If the function succeeds, the return value is the number of
     *  characters stored in the buffer pointed to by buffer, not
     *  including the terminating null character. If buffer is not
     *  large enough to hold the data, the return value is the buffer
     *  size, in characters, required to hold the string and its
     *  terminating null character and the contents of buffer are
     *  undefined. If the function fails, the return value is zero. To
     *  get extended error information, call GetLastError.
     *  @checkstyle MethodNameCheck (5 lines)
     */
    int GetEnvironmentVariable(String name, char[] buffer, int size);

    /**
     * This function returns the process identifier of the calling process.
     * @return The return value is the process identifier of the calling process.
     * @checkstyle MethodNameCheck (5 lines)
     */
    int GetCurrentProcessId();

    /**
     * Retrieves the current system date and time in Coordinated Universal Time (UTC) format.
     * @param time System time structure
     */
    void GetSystemTime(GetSystemTimeFuncCall.SystemTime time);
}
