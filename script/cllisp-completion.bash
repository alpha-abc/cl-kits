# COMP_WORDS: 类型为数组，存放当前命令行中输入的所有单词
# COMP_CWORD: 类型为整数，当前光标下输入的单词位于COMP_WORDS数组中的索引
# COMPREPLY: 类型为数组，候选的补全结果
# COMP_WORDBREAKS: 类型为字符串，表示单词之间的分隔符
# COMP_LINE: 类型为字符串，表示当前的命令行输入



## source cllisp-completion.bash
## complete -F __cllisp_main
## https://www.cnblogs.com/sandyfog/p/3635707.html
## https://blog.csdn.net/crylearner/article/details/17721615

function __cllisp_main() {
    local cur prev

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

	# 重置
    COMPREPLY=()

	case $COMP_CWORD in
	0) # 不处理根命令
		;;
	1)
    local cmd_opts="timestamp-to-date date-to-timestamp md5 json-pretty string-to-base64 base64-to-string"

    COMPREPLY=( $(compgen -W "${cmd_opts}" -- ${cur}) )
		;;
	2|*) # 等待扩充
		;;
	esac

}

complete -F __cllisp_main cl.lisp





#### make 参考, 以后完善

#    # bash completion for GNU make
#    
#    have make || have gmake || have gnumake || have pmake &&
#    _make()
#    {
#        local file makef makef_dir="." makef_inc cur prev i split=false
#    
#        COMPREPLY=()
#        _get_comp_words_by_ref cur prev
#    
#        _split_longopt && split=true
#    
#        case $prev in
#            -f|-o|-W|--file|--makefile|--old-file|--new-file|--assume-old|--assume-new|--what-if)
#                _filedir
#                return 0
#                ;;
#            -I|-C|--directory|--include-dir)
#                _filedir -d
#                return 0
#                ;;
#        esac
#    
#        $split && return 0
#    
#        if [[ "$cur" == -* ]]; then
#            COMPREPLY=( $( compgen -W '-b -m -B -C -d -e -f -h -i -I\
#                -j -l -k -n -o -p -q -r -R - s -S -t -v -w -W \
#                --always-make --directory --debug \
#                --environment-overrides --file --makefile --help \
#                --ignore-errors --include-dir --jobs --load-average \
#                --max-load --keep-going --just-print --dry-run \
#                --recon --old-file --assume-old --print-data-base \
#                --question --no-builtin-rules --no-builtin-variables \
#                --silent --quiet --no-keep-goind --stop --touch \
#                --version --print-directory --no-print-directory \
#                --what-if --new-file --assume-new \
#                --warn-undefined-variables' -- "$cur" ) )
#        else
#            # before we check for makefiles, see if a path was specified
#            # with -C/--directory
#            for (( i=0; i < ${#COMP_WORDS[@]}; i++ )); do
#                if [[ ${COMP_WORDS[i]} == -@(C|-directory) ]]; then
#                    # eval for tilde expansion
#                    eval makef_dir=${COMP_WORDS[i+1]}
#                    break
#                fi
#            done
#    
#            # before we scan for targets, see if a Makefile name was
#            # specified with -f/--file/--makefile
#            for (( i=0; i < ${#COMP_WORDS[@]}; i++ )); do
#                if [[ ${COMP_WORDS[i]} == -@(f|-?(make)file) ]]; then
#                    # eval for tilde expansion
#                    eval makef=${COMP_WORDS[i+1]}
#                    break
#                fi
#            done
#    
#            [ -n "$makef" ] && makef="-f ${makef}"
#            [ -n "$makef_dir" ] && makef_dir="-C ${makef_dir}"
#    
#            COMPREPLY=( $( compgen -W "$( make -qp $makef $makef_dir 2>/dev/null | \
#                awk -F':' '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ \
#                {split($1,A,/ /);for(i in A)print A[i]}' )" \
#                -- "$cur" ) )
#    
#        fi
#    } &&
#    complete -F _make make gmake gnumake pmake
#    
#    # Local variables:
#    # mode: shell-script
#    # sh-basic-offset: 4
#    # sh-indent-comment: t
#    # indent-tabs-mode: nil
#    # End:
#    # ex: ts=4 sw=4 et filetype=sh

