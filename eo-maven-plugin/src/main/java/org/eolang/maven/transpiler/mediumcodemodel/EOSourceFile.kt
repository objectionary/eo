package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter

/***
 * Represents an EO source code file
 * @param fileName the name of the EO source file
 * @param eoPackage the package the file belongs to
 */
class EOSourceFile(val fileName: String, val eoPackage: EOPackage) : EOSourceEntity() {

    /***
     * Package-scoped objects that belong to the source file
     */
    val objects: ArrayList<EOAbstraction> = ArrayList()

    /***
     * Adds package-scoped objects that belong to the source file
     */
    fun addObjects(vararg objects: EOAbstraction) {
        this.objects.addAll(objects)
    }

    /***
     * Each EO source code file transpiles to N target Java files,
     * where N is the number of package-scoped objects in the EO source file
     */
    override fun transpile(w: PicoWriter?): ArrayList<EOTargetFile?>? {
        val result = ArrayList<EOTargetFile?>()
        objects.forEach { result.addAll(it.transpile(null)!!) }
        return result
    }
}