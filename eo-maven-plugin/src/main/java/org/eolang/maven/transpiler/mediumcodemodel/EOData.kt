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

/**
 * Represents EO data type objects.
 */
abstract class EOData : EOSourceEntity()

/**
 * Represents integer numerals.
 */
class EOint(val value: Long) : EOData() {
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOint::class.java.simpleName}(${value}L)")
        return null
    }
}

/**
 * Represents floating-point numerals.
 */
class EOfloat(val value: Double) : EOData() {
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOfloat::class.java.simpleName}(${value}D)")
        return null
    }
}

/**
 * Represents booleans.
 */
class EObool(val value: Boolean) : EOData() {
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        w?.write("new ${EObool::class.java.simpleName}(${value})")
        return null
    }
}

/**
 * Represents strings.
 */
class EOstring(val value: String) : EOData() {
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOstring::class.java.simpleName}(\"${value}\")")
        return null
    }
}

/**
 * Represents characters.
 */
class EOchar(val value: Char) : EOData() {
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        w?.write("new ${EOchar::class.java.simpleName}('$value')")
        return null
    }
}