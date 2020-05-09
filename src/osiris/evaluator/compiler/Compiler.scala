// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.compiler

import osiris.pin.Pin

trait Compiler {

  def compile(values:Iterable[Pin[_,_]],gradients:Iterable[Pin[_,_]]):Program

}
