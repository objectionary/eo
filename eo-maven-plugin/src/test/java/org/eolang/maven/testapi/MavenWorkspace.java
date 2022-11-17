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
package org.eolang.maven.testapi;

import java.io.IOException;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Maven testing workspace.
 * @since 0.28.12
 */
public interface MavenWorkspace {

    /**
     * Adds eo program to a workspace.
     * @param program Program as a raw string.
     * @return Workspace with eo program.
     * @throws IOException If can't save eo program in workspace.
     */
    MavenWorkspace program(String program) throws IOException;

    /**
     * Executes Mojo in the workspace.
     *
     * @param mojo Mojo to execute.
     * @param <T> Template for descendants of Mojo.
     * @return Workspace after executing Mojo.
     */
    <T extends AbstractMojo> MavenWorkspace execute(Class<T> mojo);

    /**
     * Builds and returns compilation results after all manipulations.
     *
     * @return Compilation result.
     */
    CompilationResult result();
}
