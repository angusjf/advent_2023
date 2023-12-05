const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn main() !void {
    // std.debug.print("total: {d}\n", .{try solve(@embedFile("test04.txt"))});
    std.debug.print("total: {d}\n", .{try solve(@embedFile("input04.txt"))});
}

fn solve(input: []const u8) !u32 {
    var lines = std.mem.split(u8, input, "\n");

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var score = std.ArrayList(u32).init(allocator);

    while (lines.next()) |line| {
        if (std.mem.eql(u8, "", line)) {
            continue;
        }
        var parts = std.mem.split(u8, line, ": ");
        _ = parts.next() orelse return 10;
        var game = parts.next() orelse return 11;
        var split = std.mem.splitSequence(u8, game, " | ");

        var winners = split.next() orelse return 12;
        var have = split.next() orelse return 1122;
        var winners_iter = std.mem.split(u8, winners, " ");

        var winners_n = std.ArrayList(u32).init(allocator);
        defer winners_n.deinit();

        while (winners_iter.next()) |winner| {
            if (std.mem.eql(u8, "", winner)) {
                continue;
            }
            var n = try std.fmt.parseInt(u32, winner, 10);
            try winners_n.append(n);
        }

        var have_iter = std.mem.split(u8, have, " ");

        var wins: u32 = 0;

        while (have_iter.next()) |have_s| {
            if (std.mem.eql(u8, "", have_s)) {
                continue;
            }
            var n = try std.fmt.parseInt(u32, have_s, 10);
            for (winners_n.items) |winner| {
                if (n == winner) {
                    wins += 1;
                }
            }
        }

        try score.append(wins);
    }

    var card_numbers = std.ArrayList(u32).init(allocator);

    for (score.items) |_| {
        try card_numbers.append(1);
    }

    for (0.., score.items) |i, card| {
        for (i + 1..i + 1 + card) |j| {
            card_numbers.items[j] += card_numbers.items[i];
        }
    }

    var total: u32 = 0;

    for (card_numbers.items) |n| {
        total += n;
    }

    std.debug.print("{any}\n", .{card_numbers.items});
    return total;
}
