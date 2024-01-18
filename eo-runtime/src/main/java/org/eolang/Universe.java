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
package org.eolang;

/**
 * Class to manipulate eo objects within "Universe" paradigm.
 * @since 0.30
 */
@Versionized
public interface Universe {

    /**
     * Finds vertex of eo object by its location.
     * @param name Relative location of the object to find like "^.^.some-obj".
     * @return Vertex of the object to find.
     */
    int find(String name);

    /**
     * Puts data to eo object by vertex.
     * @param vertex Vertex off object.
     * @param bytes Data to put.
     * @checkstyle NonStaticMethodCheck (4 lines)
     */
    void put(int vertex, byte[] bytes);

    /**
     * Binds child to parent.
     * @param parent Vertex of the parent eo object.
     * @param child Vertex of the child eo object.
     * @param att Name of attribute.
     * @checkstyle NonStaticMethodCheck (4 lines)
     */
    void bind(int parent, int child, String att);

    /**
     * Copies the eo object.
     * @param vertex Vertex of object to copy.
     * @return Vertex of the copy.
     * @checkstyle NonStaticMethodCheck (4 lines)
     */
    int copy(int vertex);

    /**
     * Dataizes the eo object by vertex and return byte array.
     * @param vertex Vertex of eo-object.
     * @return Raw data.
     */
    byte[] dataize(int vertex);
}
