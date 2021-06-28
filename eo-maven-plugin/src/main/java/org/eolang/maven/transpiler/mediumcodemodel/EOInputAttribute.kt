/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2021 nlchar
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
package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter
import org.eolang.hse.EOarray
import org.eolang.hse.core.EOObject
import org.eolang.maven.transpiler.medium2target.TranslationCommons

/**
 * Represents an input (free) attribute.
 * @param name The name of this input attribute.
 * @param isVararg Is this input attribute variable-length?
 */
class EOInputAttribute(val name: String, val isVararg: Boolean) : EOSourceEntity() {

    /**
     * The target (Java) name of this input attribute.
     */
    val targetName: String
        get() = "EO$name"

    /**
     * The description of this input attribute (for commenting).
     */
    val description: String
        get() = "'$name' ${if (isVararg) "variable-length " else ""}free attribute"

    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        TranslationCommons.bigComment(w, "Field for storing the $description.")
        w!!.write("private final ")
        if (isVararg) {
            w.write(EOarray::class.java.simpleName)
        } else {
            w.write(EOObject::class.java.simpleName)
        }
        w.writeln(String.format(" %s;", targetName))
        return null
    }
}