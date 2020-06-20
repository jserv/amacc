CROSS_COMPILE ?= arm-linux-gnueabihf-

ARM_CC = $(CROSS_COMPILE)gcc
ARM_CC := $(shell which $(ARM_CC))                                                                                                                            
ifndef ARM_CC
$(error "no $(CROSS_COMPILE)gcc found.")
endif

ARM_CC2 = $(shell echo | $(CROSS_COMPILE)cpp -dM - | grep ARM && echo 1)
ifeq ("$(ARM_CC2)","")
$(error "no valid GNU toolchain for ARM found.")
endif

ARM_QEMU = qemu-arm
ARM_QEMU := $(shell which $(ARM_QEMU))
ifndef ARM_QEMU
$(error "no qemu-arm found. Please check package installation")
endif

# FIXME: check ld-linux.so as well
ARM_LD_LINUX_PATH := $(shell cd $(shell $(ARM_CC) --print-sysroot) 2>/dev/null && pwd)
ifeq ("$(ARM_LD_LINUX_PATH)","/") # packaged GNU toolchain
  ARM_LD_LINUX_PATH := $(shell dirname "$(shell which $(ARM_CC))")/..
  ARM_LD_LINUX_PATH := $(shell cd $(ARM_LD_LINUX_PATH) 2>/dev/null && pwd)
  ARM_LD_LINUX_PATH := $(ARM_LD_LINUX_PATH)/$(shell echo $(CROSS_COMPILE) | sed s'/.$$//')/libc
  ARM_LD_LINUX_PATH := $(shell cd $(ARM_LD_LINUX_PATH) 2>/dev/null && pwd)
  ifndef ARM_LD_LINUX_PATH
    ARM_LD_LINUX_PATH = /usr/$(shell echo $(CROSS_COMPILE) | sed s'/.$$//')
    ARM_LD_LINUX_PATH := $(shell cd $(ARM_LD_LINUX_PATH) 2>/dev/null && pwd)
  endif
endif
ifndef ARM_LD_LINUX_PATH
$(error "AMaCC requires ld-linux.so")
endif

ARM_EXEC = $(ARM_QEMU) -L $(ARM_LD_LINUX_PATH)
export ARM_EXEC
