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

package org.eolang.maven.util;

import java.util.Locale;
import java.util.Map;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;

/**
 * All the information you need to build native C library that can be used with JNI.
 *
 * @since 0.38
 */
public final class CJniInfo {
    /**
     * OS name.
     */
    private static final String OS_NAME = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);

    /**
     * Correspondence of OS and directory with platform specific JVM headers.
     */
    private static final Map<String, String> OS_TO_DIRECTORY = new MapOf<>(
        new MapEntry<>("linux", "linux"),
        new MapEntry<>("mac", "darwin"),
        new MapEntry<>("windows", "win32")
    );

    /**
     * Java Home.
     */
    private static final String JAVA_HOME = System.getProperty("java.home");

    /**
     * The jni.h header, that is common for all systems.
     */
    public static final String COMMON_HEADER = String.format("%s/include", CJniInfo.JAVA_HOME);

    /**
     * The jni_md.h header, that is platform-specific.
     */
    public static final String OS_SPEC_HEADER = String.format(
        "%s/%s", CJniInfo.COMMON_HEADER, specificIncludeDirName()
    );

    /**
     * Ctor.
     */
    private CJniInfo() { }

    /**
     * The name of the directory that contains the platform-specific C header for JNI.
     * @link <a href="https://mail.openjdk.org/pipermail/discuss/2011-June/001918.html">Where to find jni_md.h</a>
     * @return The directory name.
     */
    private static String specificIncludeDirName() {
        for (final Map.Entry<String, String> entry : CJniInfo.OS_TO_DIRECTORY.entrySet()) {
            if (CJniInfo.OS_NAME.contains(entry.getKey())) {
                return entry.getValue();
            }
        }
        throw new IllegalStateException("Unavailable OS for native C standard lib usage");
    }
}
