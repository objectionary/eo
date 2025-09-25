<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes"/>
  <xsl:variable name="pkg" select="normalize-space(/object/metas/meta[head='package'][1]/tail)"/>
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@base[starts-with(., 'Φ.')]">
    <xsl:variable name="seg" select="tokenize(., '\.')[last()]"/>
    <xsl:attribute name="base" select="concat($pkg, '.', $seg)"/>
  </xsl:template>
  <xsl:template match="metas">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
      <xsl:for-each-group select="//@base[starts-with(., 'Φ.')]" group-by="tokenize(., '\.')[last()]">
        <xsl:variable name="seg" select="current-grouping-key()"/>
        <xsl:variable name="new" select="concat($pkg, '.', $seg)"/>
        <xsl:if test="not(/object/metas/meta[head='alias' and tail=$new])">
          <meta>
            <head>alias</head>
            <tail><xsl:value-of select="$new"/></tail>
            <part><xsl:value-of select="$seg"/></part>
          </meta>
        </xsl:if>
      </xsl:for-each-group>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
