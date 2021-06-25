package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter

/***
 * Represents an EO source code package with a fully qualified name
 * @param packageName the fully qualified name of the package
 */
class EOPackage(val packageName: String) : EOSourceEntity() {

    /***
     * Files that belong to the package
     */
    val files = ArrayList<EOSourceFile>()

    /***
     * Add files that belong to the package
     */
    fun addFile(vararg files: EOSourceFile) {
        this.files.addAll(files)
    }

    /***
     * Packages are not transpiled in the current implementation of the transpiler
     */
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {return null}
}