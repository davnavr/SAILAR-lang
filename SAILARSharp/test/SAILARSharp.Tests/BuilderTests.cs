namespace SAILARSharp.Tests;

using NUnit.Framework;
using SAILARSharp.Core;
using SAILARSharp.Interop;

public sealed class BuilderTests {
    [Test]
    public void EmptyReaderCanBeWrittenToBuffer() {
        using Builder builder = Builder.Create();
        using Buffer buffer = builder.ToBuffer();
        byte[] bytes = buffer.ToArray();
        Assert.AreEqual(bytes.Length, 9);
    }
}
