<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2022 Yegor Bugayenko

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="pre-called-objects" version="2.0">
  <!--
    When we detect a call of an abstract object with
    vararg as free attribute :
    [] > test
      [] > app
        (f 1 2 3) > @
      [args...] > f
        args.length > @

    We transform the vararg attribute to an array as below :
    [] > test
      [] > app
        (f (* 1 2 3)) > @
      [args...] > f
        args.length > @
  -->
  <xsl:output encoding="UTF-8"/>
  <xsl:template name="convert-vararg-to-array">
    <xsl:param name="called-obj"/>
    <xsl:param name="free-args"/>
    <xsl:choose>
      <xsl:when test="count($free-args) &gt; 0 and count($called-obj/*) &gt;= count($free-args)">
        <xsl:copy>
          <xsl:apply-templates select="$called-obj/@*"/>
          <xsl:for-each select="$free-args">
            <xsl:variable name="pos" select="position()"/>
            <xsl:choose>
              <xsl:when test="not(@vararg)">
                <xsl:variable name="arg" select="$called-obj/o[$pos]"/>
                <xsl:element name="o">
                  <xsl:apply-templates select="$arg/node()|$arg/@*"/>
                </xsl:element>
              </xsl:when>
              <xsl:otherwise>
                <xsl:element name="o">
                  <xsl:attribute name="base">
                    <xsl:text>org.eolang.array</xsl:text>
                  </xsl:attribute>
                  <xsl:attribute name="data">
                    <xsl:text>array</xsl:text>
                  </xsl:attribute>
                  <xsl:attribute name="line">
                    <xsl:value-of select="$called-obj/@line"/>
                  </xsl:attribute>
                  <xsl:attribute name="unvaring"/>
                  <xsl:for-each select="$called-obj/o[position() &gt;= $pos]">
                    <xsl:copy>
                      <xsl:apply-templates select="node()|@*"/>
                    </xsl:copy>
                  </xsl:for-each>
                </xsl:element>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="o[@base and @ref and not(@atom) and not(@cut) and not(@ancestors) and not(@parent) and count(./*) &gt; 0]">
    <xsl:variable name="called-obj" select="."/>
    <xsl:call-template name="convert-vararg-to-array">
      <xsl:with-param name="called-obj" select="$called-obj"/>
      <xsl:with-param name="free-args" select="//objects/o[@line = $called-obj/@ref and @original-name = $called-obj/@base]/o[not(@base) and not(@level)]"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
