/* Grep through files list, suppress duplicates. */
/* Reads from stdin, outputs to stdout. */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

typedef enum { false, true } bool;

typedef struct _input_args
{
    bool ignore_case;
    bool show_lines;
    char* search_string;
} input_args;

// One more character for \n in the end of line:
#define BUF_SIZE (PATH_MAX + 1)

typedef struct _parsing_ctx
{
    input_args arguments;

    size_t search_string_len;
    char *compare_line;

    char compare_buf[BUF_SIZE];
    char show_line[BUF_SIZE];
    char matched_substr[BUF_SIZE];
    unsigned line_num;
    size_t matched_len;
    const char* substr;
    bool matched;
} parsing_ctx;

// Converts string to lower case
static void str_to_lower(char* str)
{
    int i;
    for (i = 0; str[i] != 0; ++i)
        str[i] = tolower(str[i]);
}

static void process_new_matched(parsing_ctx* ctx)
{
    assert(ctx != NULL);
    // save string part including matched substring to matched_substr:
    char* slash = strchr(ctx->substr + ctx->search_string_len, '/');
    if (!slash)
        ctx->matched_len = strlen(ctx->compare_line) - 1;
    else
        ctx->matched_len = slash - ctx->compare_line + 1;
    strncpy(ctx->matched_substr, ctx->show_line, ctx->matched_len);
    ctx->matched_substr[ctx->matched_len] = 0;

    // display line numbers if needed:
    if (ctx->arguments.show_lines)
        printf("%u-", ctx->line_num);
}

static void complete_previous_matched(const parsing_ctx* ctx)
{
    assert(ctx != NULL);
    // complete previous line:
    if (ctx->arguments.show_lines)
        printf("%u: ", ctx->line_num - 1);
    printf("%s\n", ctx->matched_substr);
}

static bool ParseCmdLineArgsSucceeded(int argc, char* const argv[], input_args* arguments)
{
    assert(argv != NULL);
    assert(arguments != NULL);
    arguments->ignore_case = false;
    arguments->show_lines = false;

    static const char* opt_string = "+in";
    int ret = getopt(argc, argv, opt_string);
    while (ret != -1)
    {
        switch (ret)
        {
            case 'i':
                arguments->ignore_case = true;
                break;
            case 'n':
                arguments->show_lines = true;
                break;
            default:
                return false;
        }
        ret = getopt(argc, argv, opt_string);
    }
    if (argc != optind + 1)
        return false;
    arguments->search_string = argv[optind];
    return true;
}

static void ParseCmdLineArgs(int argc, char* const argv[], input_args* arguments)
{
    static const char* USAGE = "\
dgrep - grep through files list, suppress duplicates.\n\
dgrep reads from stdin and outputs to stdout.\n\
\n\
Usage: dgrep [-i] [-n] STRING\n\
-i   ignore case\n\
-n   output line number\n";

    if(!ParseCmdLineArgsSucceeded(argc, argv, arguments))
    {
        fputs(USAGE, stderr);
        exit(EXIT_FAILURE);
    }
}

static void InitCtx(int argc, char* const argv[], parsing_ctx* ctx)
{
    assert(ctx != NULL);
    ParseCmdLineArgs(argc, argv, &ctx->arguments);
    ctx->search_string_len = strlen(ctx->arguments.search_string);

    if (ctx->arguments.ignore_case)
    {
        str_to_lower(ctx->arguments.search_string);
         ctx->compare_line = ctx->compare_buf;
    }
    else
        ctx->compare_line = ctx->show_line;
    ctx->line_num = 0;
    ctx->matched = false;
}

static bool ReadStringFromStream(FILE* stream, parsing_ctx* ctx)
{
    // TODO fgets() last line in file
    assert(stream != NULL);
    assert(ctx != NULL);
    if (feof(stream))
        return false;
    ++ctx->line_num;
    ctx->show_line[0] = 0;
    if (fgets(ctx->show_line, BUF_SIZE, stream) == NULL)
    {
        if (ferror(stream))
        {
            perror("fgets");
            exit(1);
        }
    }
    if(ctx->show_line[0] == 0)
        return false;
    else
        return true;
}

void InitCompareLine(parsing_ctx* ctx)
{
    assert(ctx != NULL);
    if (ctx->arguments.ignore_case)
    {
        strcpy(ctx->compare_line, ctx->show_line);
        str_to_lower(ctx->compare_line);
    }
}

static void ProcessString(parsing_ctx* ctx)
{
    assert(ctx != NULL);
    ctx->substr = strstr(ctx->compare_line, ctx->arguments.search_string);
    if (ctx->substr != NULL)
    {
        // Substring found
        if (!ctx->matched)
        {
            // this is first matched string
            ctx->matched = true;
            process_new_matched(ctx);
        }
        else
        {
            // Substring found, but it is not first.
            // Check if matched part is the same:
            if (strncmp(ctx->compare_line, ctx->matched_substr, ctx->matched_len) != 0)
            {
                // This is another matched string (it should be printed)
                complete_previous_matched(ctx);
                process_new_matched(ctx);
            } // else this is the same matched substring => do nothing
        }
    }
    else
    {
        // Substring not found
        if (ctx->matched)
        {
            complete_previous_matched(ctx);
            ctx->matched = false;
        } // else not matched again - do nothing
    }
}

int main(int argc, char* const argv[])
{
    parsing_ctx ctx;
    InitCtx(argc, argv, &ctx);

    // Read from stdin:
    while (ReadStringFromStream(stdin, &ctx))
    {
        InitCompareLine(&ctx);
        ProcessString(&ctx);
    }
    return EXIT_SUCCESS;
}
