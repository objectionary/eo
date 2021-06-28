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
 * Represents an EO source code file.
 * @param fileName The name of the EO source file.
 * @param eoPackage The package the file belongs to.
 */
class EOSourceFile(val fileName: String, val eoPackage: EOPackage) : EOSourceEntity() {

    /**
     * Package-scoped objects that belong to the source file.
     */
    val objects: ArrayList<EOAbstraction> = ArrayList()

    /**
     * Adds package-scoped objects that belong to the source file.
     */
    fun addObjects(vararg objects: EOAbstraction) {
        this.objects.addAll(objects)
    }

    /**
     * Each EO source code file transpiles to N target Java files,
     * where N is the number of package-scoped objects in the EO source file.
     */
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        val result = ArrayList<EOTargetFile?>()
        objects.forEach { result.addAll(it.transpile(PicoWriter())!!) }
        return result
    }
}