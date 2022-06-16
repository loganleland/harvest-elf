# Harvest ELF <img src="./glitched_elf.png" width="200" align=right>
[![MIT License][li]][ll]

Harvest ELF provides a datatype for the ELF format[^1] that leverages Data.Binary[^2] to provide a fast, accurate typed representation of ELF designed to be resilient against maliciously crafted ELF files.

### TODO
- Construct a serializer
- Add utility such as renaming sections and adding sections
- Provide a segment view
- Segment types have overlap in value - need to consider other information to find the most accurate segment type.

### Tested on
- LLVM output[^3]
- Linux Tsunami backdoor[^4]

[li]: https://img.shields.io/badge/License-MIT-yellow.svg
[ll]: https://opensource.org/licenses/MIT

[^1]: https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
[^2]: https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html
[^3]: https://llvm.org
[^4]: https://www.virustotal.com/gui/file/15c7fe5a56b80a43544c3227a8589045bf67d0a65c2ebba2506102250f6da963
