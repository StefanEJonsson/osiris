// SPDX-License-Identifier: GPL-2.1
//Copyright © 2020 Stefán E. Jónsson

package osiris.evaluator.compiler

import osiris.evaluator.machine.Instruction
import osiris.pin.Pin

case class Program(computations:Array[Instruction],
                   usages:Map[Either[Pin[_,_],Pin[_,_]],Int])
