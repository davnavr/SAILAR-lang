namespace SAILARSharp.Tests {
    using NUnit.Framework;
    using SAILARSharp;
    using SAILARSharp.Reader;

    public class ReaderTests {
        [Test]
        public void BasicReaderTest() {
            var contents = new byte[] {
                (byte)'S', (byte)'A', (byte)'I', (byte)'L', (byte)'A', (byte)'R',
                0, 12, 0,
                3, // Number of records
                2, 6, (byte)'H', (byte)'e', (byte)'l', (byte)'l', (byte)'o', (byte)'!',
                3, 1, 0x14,
                1, 8, 5, 2,
                3, 0xA, 0xB, 0xC,
                1, 0xFF
            };

            using var reader = new ModuleReader(contents);
            var format = reader.GetModuleFormat();
            var identifierRecord = reader.ReadNextRecord() as IdentifierRecord;
            var typeRecord = reader.ReadNextRecord() as TypeSignatureRecord;
            var dataRecord1 = reader.ReadNextRecord() as DataRecord;
            var dataRecord2 = reader.ReadNextRecord() as DataRecord;

            Assert.AreEqual(0, format.GetMajorFormatVersion());
            Assert.AreEqual(12, format.GetMinorFormatVersion());
            Assert.AreEqual(1, format.GetIntegerByteSize());
            Assert.AreEqual("Hello!", identifierRecord?.ToString());
            Assert.AreEqual(TypeSignatureKind.S32, typeRecord?.Content.Kind);
            Assert.AreEqual(new byte[] { 0xA, 0xB, 0xC }, dataRecord1?.ToArray());
            Assert.AreEqual(new byte[] { 0xFF }, dataRecord2?.ToArray());
            Assert.DoesNotThrow(() => reader.CheckIfFinished());
        }
    }
}
