package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter
import org.eolang.hse.EOarray
import org.eolang.hse.core.EOObject
import org.eolang.maven.transpiler.medium2target.TranslationCommons

/***
 * Represents an input (free) attribute
 * @param name The name of this input attribute.
 * @param isVararg Is this input attribute variable-length?
 */
class EOInputAttribute(val name: String, val isVararg: Boolean) : EOSourceEntity() {
    /***
     * The target (Java) name of this input attribute.
     */
    val targetName: String
        get() = "EO$name"

    /***
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