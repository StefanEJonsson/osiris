// SPDX-License-Identifier: GPL-2.1
//Copyright © 2019 Stefán E. Jónsson

package osiris.utilities.serialization

object Serialization {

  val version:Byte = 2

  def checkVersion(version:Byte):Unit = if(version > Serialization.version) {throw new VersionException(version)}

  class VersionException(version:Byte)
    extends Exception(
      s"Trying to parse serialization of version $version. Only supports versions up to ${Serialization.version}"
    )


}
