// For use with ImHex

char magic[7] @ 0x0;

enum IntegerSize : u8 {
	I1 = 0,
	I2 = 1,
	I4 = 2,
};

IntegerSize integer_size @ 0x7;

struct UInteger {
	if (integer_size == IntegerSize::I4) {
		u32 value;	
	}
	else if (integer_size == IntegerSize::I2) {
		u16 value;
	}
	else {
		u8 value;
	}
};

struct FormatVersion {
	UInteger major;
	UInteger minor;
};

FormatVersion format_version @ 0x8;
UInteger data_count @ 0x10;

struct ModuleData {
	UInteger byte_length;
	u8 data[byte_length.value];
};

ModuleData module_data[data_count.value] @ 0x14;

fn main() {
	std::assert(magic == "binmdl", "Not a valid module file");
};
