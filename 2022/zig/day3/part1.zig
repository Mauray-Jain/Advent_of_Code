const std = @import("std");

const MyError = error{UnsupportedChar};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const inp = try std.fs.cwd().openFile("inp.txt", .{});
    defer inp.close();
    var buf: [10000]u8 = undefined;
    const bytes_read = try inp.reader().readAll(&buf);
    const content = buf[0..(bytes_read - 1)];

    var iter = std.mem.splitScalar(u8, content, '\n');
    var sum: u16 = 0;
    while (true) {
        const rucksack: []const u8 = iter.next() orelse break;
        const compartment1: []const u8 = rucksack[0..(rucksack.len / 2)];
        const compartment2: []const u8 = rucksack[(rucksack.len / 2)..];
        for (0..compartment1.len) |i| {
            if (std.mem.indexOf(u8, compartment2, compartment1[i .. i + 1]) != null) {
                sum += try getPriority(compartment1[i]);
                try stdout.print("Char: {c}\n", .{compartment1[i]});
                break;
            }
        }
    }
    try stdout.print("Sum: {}\n", .{sum});
}

fn getPriority(ch: u8) MyError!u8 {
    if (ch >= 'a' and ch <= 'z') {
        return ch - 'a' + 1;
    } else if (ch >= 'A' and ch <= 'Z') {
        return ch - 'A' + 27;
    } else {
        return MyError.UnsupportedChar;
    }
}
